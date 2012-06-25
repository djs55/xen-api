(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Stringext
open Listext

open Xmlrpc_client
open Db_filter_types
open Network_interface

module D=Debug.Debugger(struct let name="network" end)
open D

let make_rpc path  =
	let module Rpc = struct
		let transport = ref (Unix path)
		let retrying = ref false
		let rec rpc call =
			let response' =
				try
					let response =
						XMLRPC_protocol.rpc ~srcstr:"xapi" ~dststr:"networkd" ~transport:!transport
							~http:(xmlrpc ~version:"1.0" "/") call in
					if !retrying then begin
						debug "Successfully communicated with service at %s after retrying!" path;
						retrying := false
					end;
					Some response
				with Unix.Unix_error (code, _, _) as e ->
					if code = Unix.ECONNREFUSED || code = Unix.ENOENT then begin
						if not !retrying then
							error "Could not reach the service at %s. Retrying every second..." path;
						Thread.delay 1.;
						retrying := true;
						None
					end else begin
						retrying := false;
						raise e
					end
			in
			match response' with
			| Some response -> response
			| None -> rpc call
	end in
	(module Rpc : RPC)

module Rpc = (val (make_rpc (Filename.concat Fhs.vardir "xcp-networkd")) : RPC)
module Net = Client(Rpc)

(* Catch any uncaught networkd exceptions and transform into the most relevant XenAPI error.
   We do not want a XenAPI client to see a raw network error. *)
let transform_networkd_exn pif f =
	let reraise code params =
		error "Re-raising as %s [ %s ]" code (String.concat "; " params);
		raise (Api_errors.Server_error(code, params)) in
	try
		f ()
	with
	| Script_missing script ->
		let e = Printf.sprintf "script %s missing" script in
		reraise Api_errors.pif_configuration_error [Ref.string_of pif; e]
	| Script_error params ->
		let e = Printf.sprintf "script error [%s]" (String.concat ", "
			(List.map (fun (k, v) -> k ^ " = " ^ v) params)) in
		reraise Api_errors.pif_configuration_error [Ref.string_of pif; e]
	| Read_error file | Write_error file ->
		let e = "failed to access file " ^ file in
		reraise Api_errors.pif_configuration_error [Ref.string_of pif; e]
	| Not_implemented ->
		let e = "networkd function not implemented" in
		reraise Api_errors.pif_configuration_error [Ref.string_of pif; e]
	| e ->
		error "Caught %s while trying to talk to xcp-networkd" (ExnHelper.string_of_exn e);
		reraise Api_errors.pif_configuration_error [Ref.string_of pif; ""]

let driver_domain_devs ~__context other_config =
	let devs =
		if List.mem_assoc "managed-pif-uuids" other_config then begin
			try
				let index = ref 0 in
				let devs = List.filter_map (fun uuid ->
					let pif = Db.PIF.get_by_uuid ~__context ~uuid in
					if Db.PIF.get_physical ~__context ~self:pif then begin
						let metrics = Db.PIF.get_metrics ~__context ~self:pif in
						let path = Db.PIF_metrics.get_pci_bus_path ~__context ~self:metrics in
						let dev = Some ((string_of_int !index) ^ "/" ^ path) in
						index := !index + 1;
						dev
					end else
						None
				) (String.split ',' (List.assoc "managed-pif-uuids" other_config)) in
				debug "PCIs for managed PIFs: %s" (String.concat ", " devs);
				devs
			with e ->
				warn "Could not find PCI IDs for managed PIFs";
				[]
		end else []
	in
	List.fold_left (fun acc dev ->
		try
			Pciops.of_string dev :: acc
		with _ -> acc
	) [] devs

let get_bond pif_rc =
	match pif_rc.API.pIF_bond_master_of with
	| [] -> None
	| bond :: _ ->
		Some bond

let get_vlan pif_rc =
	if pif_rc.API.pIF_VLAN_master_of = Ref.null then
		None
	else
		Some pif_rc.API.pIF_VLAN_master_of

let get_tunnel pif_rc =
	if pif_rc.API.pIF_tunnel_access_PIF_of = [] then
		None
	else
		Some (List.hd pif_rc.API.pIF_tunnel_access_PIF_of)

let get_pif_type pif_rc =
	match get_vlan pif_rc with
	| Some vlan -> `vlan_pif vlan
	| None ->
		match get_bond pif_rc with
		| Some bond -> `bond_pif bond
		| None ->
			match get_tunnel pif_rc with
			| Some tunnel -> `tunnel_pif tunnel
			| None -> `phy_pif

let rec get_base_pif ~__context pif =
	let pif_rc = Db.PIF.get_record ~__context ~self:pif in
	match get_pif_type pif_rc with
	| `tunnel_pif tunnel ->
		let slave = Db.Tunnel.get_transport_PIF ~__context ~self:tunnel in
		get_base_pif ~__context slave
	| `vlan_pif vlan ->
		let slave = Db.VLAN.get_tagged_PIF ~__context ~self:vlan in
		get_base_pif ~__context slave
	| `bond_pif bond ->
		Db.Bond.get_slaves ~__context ~self:bond
	| `phy_pif  ->
		[pif]

let get_driver_domain ~__context pif =
	let host_uuid = Xapi_inventory.lookup Xapi_inventory._installation_uuid in
	let host = Db.Host.get_by_uuid ~__context ~uuid:host_uuid in
	let local_running_vms = Db.VM.get_refs_where ~__context ~expr:(And (
		Eq (Field "power_state", Literal "Running"),
		Eq (Field "resident_on", Literal (Ref.string_of host))
	)) in
	let pif' = List.hd (get_base_pif ~__context pif) in
	let driver_domain = List.filter (fun self ->
		let oc = Db.VM.get_other_config ~__context ~self in
		List.mem_assoc "managed-pif-uuids" oc &&
		(List.exists (fun uuid -> Db.PIF.get_uuid ~__context ~self:pif' = uuid)
			(String.split ',' (List.assoc "managed-pif-uuids" oc)))
	) local_running_vms in
	match driver_domain with
	| [] -> None
	| vm :: _ ->
		debug "Network driver domain: %s" (Ref.string_of vm);
		Some vm


(*
 * Copyright (C) 2011 Citrix Systems Inc.
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

module D=Debug.Debugger(struct let name="storage_migrate" end)
open D

open Listext
open Fun
open Stringext
open Pervasiveext
open Xmlrpc_client

let local_url = ref (Http.Url.File ({ Http.Url.path = "/var/xapi/storage" }, "/"))

let rpc url call =
	XMLRPC_protocol.rpc ~transport:(transport_of_url url)
		~http:(xmlrpc ~version:"1.0" ?auth:(Http.Url.auth_of url) (Http.Url.uri_of url)) call


open Storage_interface

let success = function
	| Success x -> x
	| Failure f -> failwith (Printf.sprintf "Storage_interface.Failure %s" (f |> rpc_of_failure_t |> Jsonrpc.to_string))

let _vdi = function
	| Vdi x -> x
	| x -> failwith (Printf.sprintf "type-error, expected Vdi received %s" (x |> rpc_of_success_t |> Jsonrpc.to_string))

let _vdis = function
	| Vdis x -> x
	| x -> failwith (Printf.sprintf "type-error, expected Vdis received %s" (x |> rpc_of_success_t |> Jsonrpc.to_string))

let params = function
	| Params x -> x
	| x -> failwith (Printf.sprintf "type-error, expected Params received %s" (x |> rpc_of_success_t |> Jsonrpc.to_string))

let unit = function
	| Unit -> ()
	| x -> failwith (Printf.sprintf "type-error, expected Unit received %s" (x |> rpc_of_success_t |> Jsonrpc.to_string))

let with_activated_disk ~task ~sr ~vdi f =
	let path =
		Opt.map (fun vdi -> 
			let path = Client.VDI.attach (rpc !local_url) ~task ~dp:"migrate" ~sr ~vdi ~read_write:false |> success |> params in
			Client.VDI.activate (rpc !local_url) ~task ~dp:"migrate" ~sr ~vdi |> success |> unit;
			path) vdi in
	finally
		(fun () -> f path)
		(fun () ->
			Opt.iter
				(fun vdi ->
					Client.VDI.deactivate (rpc !local_url) ~task ~dp:"migrate" ~sr ~vdi |> success |> unit;
					Client.VDI.detach (rpc !local_url) ~task ~dp:"migrate" ~sr ~vdi |> success |> unit)
				vdi)

let export ~task ~sr ~vdi ~url ~dest =
	let remote_url = Http.Url.of_string url in
	(* Check the remote SR exists *)
	let srs = Client.SR.list (rpc remote_url) ~task in
	if not(List.mem dest srs)
	then failwith (Printf.sprintf "Remote SR %s not found" dest);
	(* Find the local VDI *)
	let vdis = Client.SR.scan (rpc !local_url) ~task ~sr |> success |> _vdis in
	let local_vdi =
		try List.find (fun x -> x.vdi = vdi) vdis
		with Not_found -> failwith (Printf.sprintf "Local VDI %s not found" vdi) in
	(* Finding VDIs which are similar to [vdi] *)
	let vdis = Client.VDI.similar_content (rpc !local_url) ~task ~sr ~vdi |> success |> _vdis in
	(* Choose the "nearest" one *)
	let nearest = List.fold_left
		(fun acc vdi -> match acc with
			| Some x -> Some x
			| None ->
				try
					let remote_vdi = Client.VDI.get_by_content (rpc remote_url) ~task ~sr:dest ~content_id:vdi.content_id |> success |> _vdi in
					debug "Local VDI %s has same content_id (%s) as remote VDI %s" vdi.vdi vdi.content_id remote_vdi.vdi;
					Some (vdi, remote_vdi)
				with _ -> None) None vdis in

	
	let dest_vdi =
		match nearest with
			| Some (_, remote_vdi) ->
				debug "Cloning remote VDI %s" remote_vdi.vdi;
				Client.VDI.clone (rpc remote_url) ~task ~sr:dest ~vdi:remote_vdi.vdi ~params:[] |> success |> _vdi
			| None ->
				debug "Creating a blank remote VDI";
				Client.VDI.create (rpc remote_url) ~task ~sr:dest ~vdi_info:local_vdi ~params:[] |> success |> _vdi in
	debug "Will copy into new remote VDI: %s" dest_vdi.vdi;
	let dest_vdi_url = Printf.sprintf "http://root:xenroot@st30.uk.xensource.com/import_raw_vdi?vdi=%s" dest_vdi.vdi in
	let base_vdi = Opt.map (fun x -> (fst x).vdi) nearest in
	debug "Will base our copy from: %s" (Opt.default "None" base_vdi);
	with_activated_disk ~task ~sr ~vdi:base_vdi
		(fun base_path ->
			with_activated_disk ~task ~sr ~vdi:(Some vdi)
				(fun src ->
					let args = [
						"-src"; Opt.unbox src;
						"-dest"; dest_vdi_url;
						"-size"; Int64.to_string dest_vdi.virtual_size;
						"-prezeroed"
					] @ (Opt.default [] (Opt.map (fun x -> [ "-base"; x ]) base_path)) in

					let out, err = Forkhelpers.execute_command_get_output "/opt/xensource/libexec/sparse_dd" args in
					debug "%s:%s" out err
				)
		);
	debug "Updating remote content_id";
	Client.VDI.set_content_id (rpc remote_url) ~task ~sr:dest ~vdi:dest_vdi.vdi ~content_id:local_vdi.content_id |> success |> unit;
	Success (Vdi dest_vdi)


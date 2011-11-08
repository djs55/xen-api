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
open Threadext
open Listext
open Xenops_interface
open Xenops_server_plugin
open Xenops_utils

module Domain = struct
	type t = {
		domid: int;
		uuid: string;
		hvm: bool;
		domain_action_request: domain_action_request option;
		paused: bool;
		built: bool;
		qemu_created: bool;
		suspended: bool;
		vbds: Vbd.t list;
		vifs: Vif.t list;
	} with rpc
end

module DB = TypedTable(struct
	include Domain
	let namespace = "domain"
end)

let next_domid =
	let next = ref None in (* unknown *)
	let get_next =
		fun () -> match !next with
			| None ->
				let domains = List.filter_map (fun x -> DB.read [x]) (DB.list []) in
				let domids = List.map (fun x -> x.Domain.domid) domains in
				let highest = List.fold_left max 0 domids in
				next := Some (highest + 1);
				highest + 1
			| Some x -> x in
	let incr_next () = next := Opt.map (fun x -> x + 1) !next in
	fun () ->
		let result = get_next () in
		incr_next ();
		debug "using domid %d" result;
		result

let key_of vm = [ vm.Vm.id ]

let m = Mutex.create ()

let read x = match DB.read x with
	| None ->
		debug "Failed to find key: %s" (String.concat "/" x);
		raise (Exception Does_not_exist)
	| Some y -> y

let create_nolock vm () =
	let k = key_of vm in
	if DB.exists k then begin
		debug "VM.create_nolock %s: Already_exists" vm.Vm.id;
		raise (Exception Already_exists)
	end else begin
		let open Domain in
		let domain = {
			domid = next_domid ();
			uuid = vm.Vm.id;
			hvm = (match vm.Vm.ty with Vm.HVM _ -> true | _ -> false);
			domain_action_request = None;
			paused = true;
			built = false;
			qemu_created = false;
			suspended = false;
			vifs = [];
			vbds = [];
		} in
		DB.write k domain
	end

let get_state_nolock vm () =
	let k = key_of vm in
	if DB.exists k then begin
		let d = read k in
		{ halted_vm with
			Vm.power_state = Running;
			domids = [ d.Domain.domid ];
		}
	end else halted_vm

let get_domain_action_request_nolock vm () =
	let k = key_of vm in
	if DB.exists k then begin
		let d = read k in
		d.Domain.domain_action_request
	end else Some Needs_poweroff

let destroy_nolock vm () =
	let k = key_of vm in
	(* Idempotent *)
	if DB.exists k then DB.delete k

let build_nolock vm vbds vifs () =
	let k = key_of vm in
	debug "setting built <- true";
	DB.write k { read k with Domain.built = true }

let create_device_model_nolock vm () =
	let k = key_of vm in
	DB.write k { read k with Domain.qemu_created = true }

let suspend_nolock vm disk () =
	let k = key_of vm in
	DB.write k { read k with Domain.suspended = true }

let resume_nolock vm disk () =
	let k = key_of vm in
	DB.write k { read k with Domain.built = true }

let do_pause_unpause_nolock vm paused () =
	let k = key_of vm in
	let d = read k in
	if not d.Domain.built || (d.Domain.hvm && not(d.Domain.qemu_created))
	then raise (Exception Domain_not_built)
	else DB.write k { d with Domain.paused = paused }

let add_vif vm vif () =
	let k = [ vm ] in
	let d = read k in
	let existing_positions = List.map (fun vif -> vif.Vif.position) d.Domain.vifs in
	if List.mem vif.Vif.position existing_positions then begin
		debug "VIF.plug %s.%s: Already exists" (fst vif.Vif.id) (snd vif.Vif.id);
		raise (Exception Already_exists)
	end else DB.write k { d with Domain.vifs = vif :: d.Domain.vifs }

let add_vbd (vm: Vm.id) (vbd: Vbd.t) () =
	debug "add_vbd";
	let k = [ vm ] in
	let d = read k in
	(* there shouldn't be any None values in here anyway *)
	let ps = List.map (fun vbd -> vbd.Vbd.position) d.Domain.vbds in
	assert (not (List.mem None ps));
	let dns = List.map (Opt.unbox) ps in
	let indices = List.map Device_number.to_disk_number dns in
	let next_index = List.fold_left max (-1) indices + 1 in
	let next_dn = Device_number.of_disk_number d.Domain.hvm next_index in
	let this_dn = Opt.default next_dn vbd.Vbd.position in
	if List.mem this_dn dns then begin
		debug "VBD.plug %s.%s: Already exists" (fst vbd.Vbd.id) (snd vbd.Vbd.id);
		raise (Exception Already_exists)
	end else DB.write k { d with Domain.vbds = { vbd with Vbd.position = Some this_dn } :: d.Domain.vbds }

let vbd_state vm vbd () =
	let k = [ vm ] in
	if not (DB.exists k)
	then unplugged_vbd
	else
		let d = read k in
		let this_one x = x.Vbd.id = vbd.Vbd.id in
		match List.filter this_one d.Domain.vbds with
			| [ vbd ] ->
				{
					unplugged_vbd with
						Vbd.plugged = true;
						media_present = vbd.Vbd.backend <> None
				}
			| [] -> unplugged_vbd
			| _ -> assert false (* at most one *)
				

let vif_state vm vif () =
	let k = [ vm ] in
	if not (DB.exists k)
	then unplugged_vif
	else
		let d = read k in
		let this_one x = x.Vif.id = vif.Vif.id in
		match List.filter this_one d.Domain.vifs with
			| [ domain ] ->
				{
					unplugged_vif with
						Vif.plugged = true;
				}
			| [] -> unplugged_vif
			| _ -> assert false (* at most one *)

let remove_vif vm vif () =
	let k = [ vm ] in
	let d = read k in
	let this_one x = x.Vif.id = vif.Vif.id in
	if List.filter this_one d.Domain.vifs = []
	then raise (Exception Does_not_exist)
	else DB.write k { d with Domain.vifs = List.filter (fun x -> not (this_one x)) d.Domain.vifs }

let remove_vbd vm vbd () =
	let k = [ vm ] in
	let d = read k in
	let this_one x = x.Vbd.id = vbd.Vbd.id in
	if List.filter this_one d.Domain.vbds = []
	then raise (Exception Does_not_exist)
	else DB.write k { d with Domain.vbds = List.filter (fun x -> not (this_one x)) d.Domain.vbds }
	
module VM = struct
	let create vm = Mutex.execute m (create_nolock vm)
	let destroy vm = Mutex.execute m (destroy_nolock vm)
	let pause vm = Mutex.execute m (do_pause_unpause_nolock vm true)
	let unpause vm = Mutex.execute m (do_pause_unpause_nolock vm false)
	let build vm vbds vifs = Mutex.execute m (build_nolock vm vbds vifs)
	let create_device_model vm = Mutex.execute m (create_device_model_nolock vm)

	let suspend vm disk = Mutex.execute m (suspend_nolock vm disk)
	let resume vm disk = Mutex.execute m (resume_nolock vm disk)

	let get_state vm = Mutex.execute m (get_state_nolock vm)
	let get_domain_action_request vm = Mutex.execute m (get_domain_action_request_nolock vm)
end

module VBD = struct
	let plug (vm: Vm.id) (vbd: Vbd.t) = Mutex.execute m (add_vbd vm vbd)
	let unplug vm vbd = Mutex.execute m (remove_vbd vm vbd)

	let insert vm vbd disk = ()
	let eject vm vbd = ()

	let get_state vm vbd = Mutex.execute m (vbd_state vm vbd)
end

module VIF = struct
	let plug vm vif = Mutex.execute m (add_vif vm vif)
	let unplug vm vif = Mutex.execute m (remove_vif vm vif)

	let get_state vm vif = Mutex.execute m (vif_state vm vif)
end

let updates = Updates.empty ()

module UPDATES = struct
	let get last = Updates.get last updates
end

module DEBUG = struct
	let trigger cmd args = match cmd, args with
		| "reboot", k ->
			let d = read k in
			DB.write k { d with Domain.domain_action_request = Some Needs_reboot };
			Updates.add (Dynamic.Vm (List.hd k)) updates
		| "halt", k ->
			let d = read k in
			DB.write k { d with Domain.domain_action_request = Some Needs_poweroff };
			Updates.add (Dynamic.Vm (List.hd k)) updates
		| _ ->
			debug "DEBUG.trigger cmd=%s Not_supported" cmd;
			raise (Exception Not_supported)
end

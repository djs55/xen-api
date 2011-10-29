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
open Xenops_utils

module Domain = struct
	type t = {
		domid: int;
		uuid: string;
		hvm: bool;
		shutdown_reason: string option;
		paused: bool;
		built: bool;
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
		throw Does_not_exist
	| Some y -> return y

let create_nolock vm () =
	let k = key_of vm in
	if DB.exists k then begin
		debug "VM.create_nolock %s: Already_exists" vm.Vm.id;
		throw Already_exists
	end else begin
		let open Domain in
		let domain = {
			domid = next_domid ();
			uuid = vm.Vm.id;
			hvm = (match vm.Vm.ty with Vm.HVM _ -> true | _ -> false);
			shutdown_reason = None;
			paused = true;
			built = false;
			suspended = false;
			vifs = [];
			vbds = [];
		} in
		DB.write k domain;
		return ()
	end

let get_power_state_nolock vm () =
	let k = key_of vm in
	if DB.exists k then begin
		read k >>= fun d ->
		return (Running { domid = d.Domain.domid })
	end else return Halted

let destroy_nolock vm () =
	let k = key_of vm in
	if not(DB.exists k)
	then throw Does_not_exist
	else begin
		DB.delete k;
		return ()
	end

let build_nolock vm () =
	let k = key_of vm in
	read k >>= fun d ->
	debug "setting built <- true";
	DB.write k { d with Domain.built = true };
	return ()

let suspend_nolock vm disk () =
	let k = key_of vm in
	read k >>= fun d ->
	DB.write k { d with Domain.suspended = true };
	return ()

let resume_nolock vm disk () =
	let k = key_of vm in
	read k >>= fun d ->
	DB.write k { d with Domain.built = true };
	return ()

let do_pause_unpause_nolock vm paused () =
	let k = key_of vm in
	read k >>= fun d ->
	if not d.Domain.built
	then throw Domain_not_built
	else begin
		DB.write k { d with Domain.paused = paused };
		return ();
	end

let add_vif vm vif () =
	let k = [ vm ] in
	read k >>= fun d ->
	let existing_positions = List.map (fun vif -> vif.Vif.position) d.Domain.vifs in
	if List.mem vif.Vif.position existing_positions then begin
		debug "VIF.plug %s.%s: Already exists" (fst vif.Vif.id) (snd vif.Vif.id);
		throw Already_exists
	end else begin
		DB.write k { d with Domain.vifs = vif :: d.Domain.vifs };
		return ()
	end

let add_vbd (vm: Vm.id) (vbd: Vbd.t) () =
	debug "add_vbd";
	let k = [ vm ] in
	read k >>= fun d ->
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
		throw Already_exists
	end else begin
		DB.write k { d with Domain.vbds = { vbd with Vbd.position = Some this_dn } :: d.Domain.vbds };
		return ()
	end

let vbd_attached vm vbd () =
	let k = [ vm ] in
	if not (DB.exists k)
	then return false
	else
		read k >>= fun d ->
		let this_one x = x.Vbd.id = vbd.Vbd.id in
		return (List.filter this_one d.Domain.vbds <> [])

let vif_attached vm vif () =
	let k = [ vm ] in
	if not (DB.exists k)
	then return false
	else
		read k >>= fun d ->
		let this_one x = x.Vif.id = vif.Vif.id in
		return (List.filter this_one d.Domain.vifs <> [])

let remove_vif vm vif () =
	let k = [ vm ] in
	read k >>= fun d ->
	let this_one x = x.Vif.id = vif.Vif.id in
	if List.filter this_one d.Domain.vifs = []
	then throw Does_not_exist
	else begin
		DB.write k { d with Domain.vifs = List.filter (fun x -> not (this_one x)) d.Domain.vifs };
		return ()
	end

let remove_vbd vm vbd () =
	let k = [ vm ] in
	read k >>= fun d ->
	let this_one x = x.Vbd.id = vbd.Vbd.id in
	if List.filter this_one d.Domain.vbds = []
	then throw Does_not_exist
	else begin
		DB.write k { d with Domain.vbds = List.filter (fun x -> not (this_one x)) d.Domain.vbds };
		return ()
	end
	
module VM = struct
	let create vm = Mutex.execute m (create_nolock vm)
	let destroy vm = Mutex.execute m (destroy_nolock vm)
	let pause vm = Mutex.execute m (do_pause_unpause_nolock vm true)
	let unpause vm = Mutex.execute m (do_pause_unpause_nolock vm false)
	let build vm = Mutex.execute m (build_nolock vm)

	let suspend vm disk = Mutex.execute m (suspend_nolock vm disk)
	let resume vm disk = Mutex.execute m (resume_nolock vm disk)

	let get_power_state vm = Mutex.execute m (get_power_state_nolock vm)
end

module VBD = struct
	let plug (vm: Vm.id) (vbd: Vbd.t) = Mutex.execute m (add_vbd vm vbd)
	let unplug vm vbd = Mutex.execute m (remove_vbd vm vbd)

	let get_currently_attached vm vbd = Mutex.execute m (vbd_attached vm vbd)
end

module VIF = struct
	let plug vm vif = Mutex.execute m (add_vif vm vif)
	let unplug vm vif = Mutex.execute m (remove_vif vm vif)

	let get_currently_attached vm vif = Mutex.execute m (vif_attached vm vif)
end

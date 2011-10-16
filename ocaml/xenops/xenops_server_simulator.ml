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
module D = Debug.Debugger(struct let name = "xenops" end)
open D

open Threadext
open Listext
open Xenops_interface
open Xenops_utils

type domain = {
	domid: int;
	uuid: string;
	hvm: bool;
	shutdown_reason: Domain.shutdown_reason option;
	paused: bool;
	built: bool;
	vbds: Vbd.t list;
	vifs: Vif.t list;
}

module StringMap = Map.Make(struct type t = string let compare = compare end)

let uuid_to_domain = ref StringMap.empty

let next_domid =
	let next = ref 1 in
	fun () ->
		let result = !next in
		incr next;
		result

let m = Mutex.create ()

let make_nolock vm () =
	if StringMap.mem vm.Vm.id !uuid_to_domain
	then throw Already_exists
	else begin
		let domain = {
			domid = next_domid ();
			uuid = vm.Vm.id;
			hvm = (match vm.Vm.ty with Vm.HVM _ -> true | _ -> false);
			shutdown_reason = None;
			paused = true;
			built = false;
			vifs = [];
			vbds = [];
		} in
		uuid_to_domain := StringMap.add vm.Vm.id domain !uuid_to_domain;
		return ()
	end

let destroy_nolock vm () =
	if not(StringMap.mem vm.Vm.id !uuid_to_domain)
	then throw Does_not_exist
	else begin
		uuid_to_domain := StringMap.remove vm.Vm.id !uuid_to_domain;
		return ()
	end

let build_nolock vm () =
	if not(StringMap.mem vm.Vm.id !uuid_to_domain)
	then throw Does_not_exist
	else begin
		debug "setting built <- true";
		let d = StringMap.find vm.Vm.id !uuid_to_domain in
		uuid_to_domain := StringMap.add vm.Vm.id { d with built = true } !uuid_to_domain;
		return ()
	end

let do_pause_unpause_nolock vm paused () =
	if not(StringMap.mem vm.Vm.id !uuid_to_domain)
	then throw Does_not_exist
	else begin
		let d = StringMap.find vm.Vm.id !uuid_to_domain in
		if not d.built
		then throw Domain_not_built
		else begin
			uuid_to_domain := StringMap.add vm.Vm.id { d with paused = paused } !uuid_to_domain;
			return ()
		end
	end

let add_vif vm vif () =
	if not(StringMap.mem vm !uuid_to_domain)
	then throw Does_not_exist
	else begin
		let d = StringMap.find vm !uuid_to_domain in
		let existing_positions = List.map (fun vif -> vif.Vif.position) d.vifs in
		if List.mem vif.Vif.position existing_positions
		then throw Already_exists
		else begin
			uuid_to_domain := StringMap.add vm { d with vifs = vif :: d.vifs } !uuid_to_domain;
			return ()
		end
	end

let add_vbd vm vbd () =
	if not(StringMap.mem vm !uuid_to_domain)
	then throw Does_not_exist
	else begin
		let d = StringMap.find vm !uuid_to_domain in
		(* there shouldn't be any None values in here anyway *)
		let ps = List.map (fun vbd -> vbd.Vbd.position) d.vbds in
		assert (not (List.mem None ps));
		let dns = List.map (Opt.unbox) ps in
		let indices = List.map Device_number.to_disk_number dns in
		let next_index = List.fold_left max (-1) indices + 1 in
		let next_dn = Device_number.of_disk_number d.hvm next_index in
		let this_dn = Opt.default next_dn vbd.Vbd.position in
		if List.mem this_dn dns
		then throw Already_exists
		else begin
			uuid_to_domain := StringMap.add vm { d with vbds = { vbd with Vbd.position = Some this_dn } :: d.vbds } !uuid_to_domain;
			return ()
		end
	end

let remove_vif vm vif () =
	if not(StringMap.mem vm !uuid_to_domain)
	then throw Does_not_exist
	else begin
		let d = StringMap.find vm !uuid_to_domain in
		let this_one x = x.Vif.id = vif.Vif.id in
		if List.filter this_one d.vifs = []
		then throw Does_not_exist
		else begin
			uuid_to_domain := StringMap.add vm { d with vifs = List.filter (fun x -> not (this_one x)) d.vifs } !uuid_to_domain;
			return ()
		end
	end

let remove_vbd vm vbd () =
	if not(StringMap.mem vm !uuid_to_domain)
	then throw Does_not_exist
	else begin
		let d = StringMap.find vm !uuid_to_domain in
		let this_one x = x.Vbd.id = vbd.Vbd.id in
		if List.filter this_one d.vbds = []
		then throw Does_not_exist
		else begin
			uuid_to_domain := StringMap.add vm { d with vbds = List.filter (fun x -> not (this_one x)) d.vbds } !uuid_to_domain;
			return ()
		end
	end
	
module VM = struct
	let make vm = Mutex.execute m (make_nolock vm)
	let destroy vm = Mutex.execute m (destroy_nolock vm)
	let pause vm = Mutex.execute m (do_pause_unpause_nolock vm true)
	let unpause vm = Mutex.execute m (do_pause_unpause_nolock vm false)
	let build vm = Mutex.execute m (build_nolock vm)
end

module VBD = struct
	let plug vm vbd = Mutex.execute m (add_vbd vm vbd)
	let unplug vm vbd = Mutex.execute m (remove_vbd vm vbd)
end

module VIF = struct
	let plug vm vif = Mutex.execute m (add_vif vm vif)
	let unplug vm vif = Mutex.execute m (remove_vif vm vif)
end

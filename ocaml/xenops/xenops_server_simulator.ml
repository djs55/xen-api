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
open Xenops_interface
open Xenops_utils

type domain = {
	domid: int;
	uuid: string;
	shutdown_reason: Domain.shutdown_reason option;
	paused: bool;
	built: bool;
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
			shutdown_reason = None;
			paused = true;
			built = false;
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
		debug "built = %b" d.built;
		if not d.built
		then throw Domain_not_built
		else begin
			uuid_to_domain := StringMap.add vm.Vm.id { d with paused = paused } !uuid_to_domain;
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

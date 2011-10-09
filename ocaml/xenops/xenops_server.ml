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

open Listext
open Xenops_interface

type context = unit

let make_context () = ()

let query _ _ = Some {
    Query.name = "xenops";
    vendor = "XCP";
    version = "0.1";
    features = [];
}, None

let root = "/var/run/xapi/vms"
let path_of_vm vm = Printf.sprintf "%s/%s" root vm
let config_of_vm vm = Printf.sprintf "%s/config" (path_of_vm vm)

let all = List.fold_left (&&) true
let any = List.fold_left (||) false

module VM = struct
	open Vm

	let read_config vm =
		try
			Some (t_of_rpc (Jsonrpc.of_string (Unixext.string_of_file (config_of_vm vm))))
		with _ -> None

	let write_config vm =
		Unixext.mkdir_rec (path_of_vm vm.vm) 0o755;
		let json = Jsonrpc.to_string (rpc_of_t vm) in
		let filename = config_of_vm vm.vm in
		Unixext.write_string_to_file filename json

	let exists vm =
		any (List.map Sys.file_exists [ path_of_vm vm; config_of_vm vm ])

	let create _ x =
		if exists x.vm
		then None, Some (Already_exists x.vm)
		else begin
			write_config x;
			Some x.vm, None
		end

	let destroy _ vm =
		if not(exists vm)
		then None, Some (Does_not_exist vm)
		else begin
			Unixext.unlink_safe (config_of_vm vm);
			Unix.rmdir (path_of_vm vm);
			Some (), None
		end

	let list _ () =
		let ids = Array.to_list (Sys.readdir root) in
		let all = List.filter_map read_config ids in
		Some all, None
end

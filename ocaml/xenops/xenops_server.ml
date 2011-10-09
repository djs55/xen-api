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
open Stringext
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

let all = List.fold_left (&&) true
let any = List.fold_left (||) false

let wrap f =
	try
		f ()
	with e ->
		Printf.fprintf stderr "Caught: %s\n%!" (Printexc.to_string e);
		Printf.fprintf stderr "%s\n%!" (Printexc.get_backtrace ());
		raise e

module type ITEM = sig
	type t
	type id
	type create_error = Already_exists of id
	type destroy_error = Does_not_exist of id
	val id_of_t: t -> id
	val t_of_rpc: Rpc.t -> t
	val rpc_of_t: t -> Rpc.t

	val filename_of_id: id -> string
end

module FilesystemDB = functor(Item: ITEM) -> struct
	let read id =
		try
			Some (Item.t_of_rpc (Jsonrpc.of_string (Unixext.string_of_file (Item.filename_of_id id))))
		with _ -> None
	let write x =
		let filename = Item.filename_of_id (Item.id_of_t x) in
		Unixext.mkdir_rec (Filename.dirname filename) 0o755;
		let json = Jsonrpc.to_string (Item.rpc_of_t x) in
		Unixext.write_string_to_file filename json
	let exists id = Sys.file_exists (Item.filename_of_id id)
	let delete id =
		let filename = Item.filename_of_id id in
		Unixext.unlink_safe filename

	let create _ x =
		wrap
			(fun () ->
				let id = Item.id_of_t x in
				if exists id
				then None, Some (Item.Already_exists id)
				else begin
					write x;
					Some id, None
				end
			)

	let destroy _ id =
		wrap
			(fun () ->
				if not(exists id)
				then None, Some (Item.Does_not_exist id)
				else begin
					delete id;
					Some (), None
				end
			)
end

module VM = struct
	open Vm

	module DB = FilesystemDB(struct
		include Vm
		let filename_of_id id = Printf.sprintf "%s/%s/vm" root id
		let id_of_t x = x.id
	end)
	include DB

	let destroy x id =
		wrap
			(fun () ->
				if not(exists id)
				then None, Some (Does_not_exist id)
				else begin
					(* Also delete all related objects *)
					let all = Array.to_list (Sys.readdir (Printf.sprintf "%s/%s" root id)) in
					List.iter (fun x -> Unixext.unlink_safe (Printf.sprintf "%s/%s/%s" root id x)) all;
					Unix.rmdir (Printf.sprintf "%s/%s" root id);
					Some (), None
				end
			)

	let list _ () =
		wrap
			(fun () ->
				let ids = Array.to_list (Sys.readdir root) in
				let all = List.filter_map read ids in
				Some all, None
			)
end

module VBD = struct
	open Vbd

	module DB = FilesystemDB(struct
		include Vbd
		let filename_of_id (vm_id, id) = Printf.sprintf "%s/%s/vbd.%s" root vm_id id
		let id_of_t x = x.id
	end)
	include DB

	let list _ vm_id =
		wrap
			(fun () ->
				let possible_ids = Array.to_list (Sys.readdir (Printf.sprintf "%s/%s" root vm_id)) in
				let parse_vbd_id x =
					let prefix = "vbd." in
					if not(String.startswith prefix x)
					then None
					else Some(String.sub x (String.length prefix) (String.length x - (String.length prefix))) in
				let ids = List.filter_map parse_vbd_id possible_ids in
				let all = List.filter_map read (List.map (fun id -> vm_id, id) ids) in
				Some all, None
			)
end

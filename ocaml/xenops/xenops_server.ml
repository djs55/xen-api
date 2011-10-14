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
open Fun
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

exception Protocol_error

let ( >>= ) (a, b) f = match b with
	| Some error -> None, Some error
	| None ->
		begin match a with
			| None -> raise Protocol_error
			| Some x -> f x
		end
let return x = Some x, None

let need_some = function
	| Some x -> Some x, None
	| None -> None, Some Does_not_exist

let dropnone x = List.filter_map (fun x -> x) x

let wrap f =
	try
		f ()
	with e ->
		Printf.fprintf stderr "Caught: %s\n%!" (Printexc.to_string e);
		Printf.fprintf stderr "%s\n%!" (Printexc.get_backtrace ());
		raise e

module type READWRITE = sig
	type t
	val t_of_rpc: Rpc.t -> t
	val rpc_of_t: t -> Rpc.t
end

module TypedTable = functor(RW: READWRITE) -> struct
	open RW
	type key = string list
	let filename_of_key k = Printf.sprintf "%s/%s" root (String.concat "/" k)
	let read (k: key) =
		let filename = filename_of_key k in
		try
			Some (t_of_rpc (Jsonrpc.of_string (Unixext.string_of_file filename)))
		with _ -> None
	let write (k: key) (x: t) =
		let filename = filename_of_key k in
		Unixext.mkdir_rec (Filename.dirname filename) 0o755;
		let json = Jsonrpc.to_string (rpc_of_t x) in
		Unixext.write_string_to_file filename json
	let exists (k: key) = Sys.file_exists (filename_of_key k)
	let delete (k: key) =
		let filename = filename_of_key k in
		let rec rm_rf f =
			if not(Sys.is_directory f)
			then Unixext.unlink_safe f
			else begin
				List.iter rm_rf (List.map (Filename.concat f) (Array.to_list (Sys.readdir f)));
				Unix.rmdir f
			end in
		rm_rf filename
	let list (k: key) = Array.to_list (Sys.readdir (filename_of_key k))

	let create (k: key) (x: t) =
		if exists k
		then None, Some Already_exists
		else begin
			write k x;
			Some (), None
		end

	let destroy (k: key) =
		if not(exists k)
		then None, Some Does_not_exist
		else begin
			delete k;
			Some (), None
		end
end

open Xenops_server_plugin

let backend = ref None
let set_backend m = backend := m
let get_backend () = match !backend with
  | Some x -> x 
  | None -> failwith "No backend implementation set"

let foo () =
let module M = (val get_backend () : S) in
()

module VM = struct
	open Vm

	module DB = TypedTable(Vm)

	let key_of id = [ id; "config" ]
	let create _ x =
		DB.create (key_of x.id) x
		>>= fun () ->
		return x.id
	let destroy _ x = DB.destroy [ x ]
	let list _ () =
		return (DB.list [ ] |> (List.map (DB.read ++ key_of)) |> dropnone)

	let make _ id =
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		return (B.make x)

	let build _ id =
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		return (B.build x)

	let pause _ id =
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		return (B.pause x)

	let unpause _ id =
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		return (B.unpause x)
end

let filter_prefix prefix xs =
	List.filter_map
		(fun x ->
			if String.startswith prefix x
			then Some (String.sub x (String.length prefix) (String.length x - (String.length prefix)))
			else None) xs

module VBD = struct
	open Vbd

	module DB = TypedTable(Vbd)

	let create _ vbd =
		DB.create [ fst (vbd.id); "vbd." ^ (snd vbd.id) ] vbd
		>>= fun () ->
		return vbd.id
	let destroy _ (vm, vbd) = DB.destroy [ vm; "vbd." ^ vbd ]
	let list _ vm =
		let key_of id = [ vm; "vbd." ^ id ] in
		return (DB.list [ vm ] |> (filter_prefix "vbd.") |> (List.map (DB.read ++ key_of)) |> dropnone)
end

module VIF = struct
	open Vif

	module DB = TypedTable(Vif)

	let create _ vif =
		DB.create [ fst (vif.id); "vif." ^ (snd vif.id) ] vif
		>>= fun () ->
		return vif.id
	let destroy _ (vm, vif) = DB.destroy [ vm; "vif." ^ vif ]
	let list _ vm =
		let key_of id = [ vm; "vif." ^ id ] in
		return (DB.list [ vm ] |> (filter_prefix "vif.") |> (List.map (DB.read ++ key_of)) |> dropnone)
end

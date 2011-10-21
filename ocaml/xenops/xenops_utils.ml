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

module D = Debug.Debugger(struct let name = "xenops" end)

let print_debug = ref false

let debug (fmt: ('a , unit, string, unit) format4) =
	let time_of_float x = 
		let time = Unix.gmtime x in
		Printf.sprintf "%04d%02d%02dT%02d:%02d:%02dZ"
			(time.Unix.tm_year+1900)
			(time.Unix.tm_mon+1)
			time.Unix.tm_mday
			time.Unix.tm_hour
			time.Unix.tm_min
			time.Unix.tm_sec in
	if !print_debug 
	then Printf.kprintf
		(fun s -> 
			Printf.printf "%s %s\n" (time_of_float (Unix.gettimeofday ()))  s; 
			flush stdout) fmt
	else Printf.kprintf (fun s -> D.debug "%s" s) fmt

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
let throw e = None, Some e

let need_some = function
	| Some x -> Some x, None
	| None -> None, Some Does_not_exist

let dropnone x = List.filter_map (fun x -> x) x

exception Exception of error

let wrap f x =
	try
		f x
	with
		| Exception e ->
			debug "Caught: %s" (Jsonrpc.to_string (rpc_of_error e));
			throw e
		| e ->
			debug "Caught: %s" (Printexc.to_string e);
			debug "%s" (Printexc.get_backtrace ());
			throw (Internal_error (Printexc.to_string e))

module type READWRITE = sig
	type t
	val t_of_rpc: Rpc.t -> t
	val rpc_of_t: t -> Rpc.t
end

let root = "/var/run/xapi/vms"

let rec rm_rf f =
	if not(Sys.is_directory f)
	then Unixext.unlink_safe f
	else begin
		List.iter rm_rf (List.map (Filename.concat f) (Array.to_list (Sys.readdir f)));
		Unix.rmdir f
	end

let empty_database () =
	if Sys.file_exists root then rm_rf root;
	Unixext.mkdir_rec root 0x0755

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


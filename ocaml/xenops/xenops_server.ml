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
open Xenops_server_plugin
open Xenops_utils

type context = unit

let make_context () = ()

let query _ _ = Some {
    Query.name = "xenops";
    vendor = "XCP";
    version = "0.1";
    features = [];
}, None

let backend = ref None
let set_backend m = backend := m
let get_backend () = match !backend with
  | Some x -> x 
  | None -> failwith "No backend implementation set"

let filter_prefix prefix xs =
	List.filter_map
		(fun x ->
			if String.startswith prefix x
			then Some (String.sub x (String.length prefix) (String.length x - (String.length prefix)))
			else None) xs

module VBD = struct
	open Vbd

	module DB = TypedTable(struct
		include Vbd
		let namespace = "VM"
	end)

	let vm_of = fst
	let key_of k = [ fst k; "vbd." ^ (snd k) ]
	let string_of_id (a, b) = a ^ "." ^ b
	let add _ x =
		debug "VBD.add %s %s" (string_of_id x.id) (Jsonrpc.to_string (rpc_of_t x));
		DB.add (key_of x.id) x
		>>= fun () ->
		return x.id
	let plug _ id =
		debug "VBD.plug %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VBD.plug (vm_of id) x
	let unplug _ id =
		debug "VBD.unplug %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VBD.unplug (vm_of id) x
	let remove _ id =
		debug "VBD.remove %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VBD.get_currently_attached (vm_of id) x
		>>= fun attached ->
		if attached
		then throw Device_is_connected
		else DB.remove (key_of id)
	let list _ vm =
		debug "VBD.list";
		let key_of' id = [ vm; "vbd." ^ id ] in
		return (DB.list [ vm ] |> (filter_prefix "vbd.") |> (List.map (DB.read ++ key_of')) |> dropnone)
end

module VIF = struct
	open Vif

	module DB = TypedTable(struct
		include Vif
		let namespace = "VM"
	end)

	let vm_of = fst
	let key_of k = [ fst k; "vif." ^ (snd k) ]
	let string_of_id (a, b) = a ^ "." ^ b
	let add _ x =
		debug "VIF.add %s" (Jsonrpc.to_string (rpc_of_t x));
		DB.add (key_of x.id) x
		>>= fun () ->
		return x.id
	let plug _ id =
		debug "VIF.plug %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VIF.plug (vm_of id) x
	let unplug _ id =
		debug "VIF.unplug %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VIF.unplug (vm_of id) x
	let remove _ id =
		debug "VIF.remove %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VIF.get_currently_attached (vm_of id) x
		>>= fun attached ->
		if attached
		then throw Device_is_connected
		else DB.remove (key_of id)
	let list _ vm =
		let key_of' id = [ vm; "vif." ^ id ] in
		return (DB.list [ vm ] |> (filter_prefix "vif.") |> (List.map (DB.read ++ key_of')) |> dropnone)
end

module VM = struct
	open Vm

	module DB = TypedTable(struct
		include Vm
		let namespace = "VM"
	end)

	let key_of id = [ id; "config" ]
	let add _ x =
		debug "VM.add %s" (Jsonrpc.to_string (rpc_of_t x));
		DB.add (key_of x.id) x
		>>= fun () ->
		return x.id
	let remove _ id =
		debug "VM.remove %s" id;
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VM.get_power_state x
		>>= fun power -> match power with
			| Running _ | Suspended | Paused -> throw (Bad_power_state(power, Halted))
			| Halted -> DB.remove [ id ]
	let list _ () =
		debug "VM.list";
		let module B = (val get_backend () : S) in
		let rec loop acc = function
			| [] -> return acc
			| x :: xs ->
				need_some (x |> key_of |> DB.read)
				>>= fun vm_t ->
				B.VM.get_power_state vm_t
				>>= fun power ->
				loop ((vm_t, power) :: acc) xs in
		loop [] (DB.list [])

	let make _ id =
		debug "VM.make %s" id;
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VM.make x

	let build _ id =
		debug "VM.build %s" id;
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VM.build x

	let shutdown _ id =
		debug "VM.shutdown %s" id;
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VM.destroy x

	let pause _ id =
		debug "VM.pause %s" id;
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VM.pause x

	let unpause _ id =
		debug "VM.unpause %s" id;
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VM.unpause x

	let start c id =
		debug "VM.start %s" id;
		make c id
		>>= fun () ->
		build c id
		>>= fun () ->
		VBD.list c id
		>>= fun vbds ->
		iter (fun vbd -> VBD.plug c vbd.Vbd.id) vbds
		>>= fun () ->
		VIF.list c id
		>>= fun vifs ->
		iter (fun vif -> VIF.plug c vif.Vif.id) vifs

	let suspend _ id disk =
		debug "VM.suspend %s" id;
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VM.suspend x disk

	let resume _ id disk =
		debug "VM.resume %s" id;
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VM.resume x disk
end


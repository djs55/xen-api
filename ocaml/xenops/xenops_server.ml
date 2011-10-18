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

module VM = struct
	open Vm

	module DB = TypedTable(Vm)

	let key_of id = [ id; "config" ]
	let create _ x =
		DB.create (key_of x.id) x
		>>= fun () ->
		return x.id
	let destroy _ id =
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VM.get_power_state x
		>>= fun power ->
		if power = Running
		then throw (Bad_power_state(Running, Halted))
		else DB.destroy [ id ]
	let list _ () =
		return (DB.list [ ] |> (List.map (DB.read ++ key_of)) |> dropnone)

	let make _ id =
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VM.make x

	let build _ id =
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VM.build x

	let shutdown _ id =
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VM.destroy x

	let pause _ id =
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VM.pause x

	let unpause _ id =
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VM.unpause x
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

	let vm_of = fst
	let key_of k = [ fst k; "vbd." ^ (snd k) ]
	let create _ vbd =
		DB.create (key_of vbd.id) vbd
		>>= fun () ->
		return vbd.id
	let plug _ id =
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VBD.plug (vm_of id) x
	let unplug _ id =
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VBD.unplug (vm_of id) x
	let destroy _ id =
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VBD.get_currently_attached (vm_of id) x
		>>= fun attached ->
		if attached
		then throw Device_is_connected
		else DB.destroy (key_of id)
	let list _ vm =
		let key_of' id = [ vm; "vbd." ^ id ] in
		return (DB.list [ vm ] |> (filter_prefix "vbd.") |> (List.map (DB.read ++ key_of')) |> dropnone)
end

module VIF = struct
	open Vif

	module DB = TypedTable(Vif)

	let vm_of = fst
	let key_of k = [ fst k; "vif." ^ (snd k) ]
	let create _ vif =
		DB.create (key_of vif.id) vif
		>>= fun () ->
		return vif.id
	let plug _ id =
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VIF.plug (vm_of id) x
	let unplug _ id =
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VIF.unplug (vm_of id) x
	let destroy _ id =
		let module B = (val get_backend () : S) in
		need_some (id |> key_of |> DB.read)
		>>= fun x ->
		B.VIF.get_currently_attached (vm_of id) x
		>>= fun attached ->
		if attached
		then throw Device_is_connected
		else DB.destroy (key_of id)
	let list _ vm =
		let key_of' id = [ vm; "vif." ^ id ] in
		return (DB.list [ vm ] |> (filter_prefix "vif.") |> (List.map (DB.read ++ key_of')) |> dropnone)
end

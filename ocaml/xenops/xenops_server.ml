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
		DB.add (key_of x.id) x;
		return x.id
	let plug _ id =
		debug "VBD.plug %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		B.VBD.plug (vm_of id) (id |> key_of |> DB.read |> unbox) |> return
	let unplug _ id =
		debug "VBD.unplug %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		B.VBD.unplug (vm_of id) (id |> key_of |> DB.read |> unbox) |> return
	let remove _ id =
		debug "VBD.remove %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		if B.VBD.get_currently_attached (vm_of id) (id |> key_of |> DB.read |> unbox)
		then raise (Exception Device_is_connected)
		else return (DB.remove (key_of id))
	let list _ vm =
		debug "VBD.list";
		let key_of' id = [ vm; "vbd." ^ id ] in
		DB.list [ vm ] |> (filter_prefix "vbd.") |> (List.map (DB.read ++ key_of')) |> dropnone |> return
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
		(* Generate MAC if necessary *)
		let mac = match x.mac with
			| "random" -> Device.Vif.random_local_mac ()
			| "" -> Device.Vif.hashchain_local_mac x.position (vm_of x.id)
			| mac -> mac in
		DB.add (key_of x.id) { x with mac = mac };
		return x.id
	let plug _ id =
		debug "VIF.plug %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		B.VIF.plug (vm_of id) (id |> key_of |> DB.read |> unbox) |> return
	let unplug _ id =
		debug "VIF.unplug %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		B.VIF.unplug (vm_of id) (id |> key_of |> DB.read |> unbox) |> return
	let remove _ id =
		debug "VIF.remove %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		if B.VIF.get_currently_attached (vm_of id) (id |> key_of |> DB.read |> unbox)
		then raise (Exception Device_is_connected)
		else return (DB.remove (key_of id))
	let list _ vm =
		let key_of' id = [ vm; "vif." ^ id ] in
		DB.list [ vm ] |> (filter_prefix "vif.") |> (List.map (DB.read ++ key_of')) |> dropnone |> return
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
		DB.add (key_of x.id) x;
		return x.id
	let remove _ id =
		debug "VM.remove %s" id;
		let module B = (val get_backend () : S) in
		let power = B.VM.get_power_state (id |> key_of |> DB.read |> unbox) in
		match power with
			| Running _ | Suspended | Paused -> raise (Exception (Bad_power_state(power, Halted)))
			| Halted ->
				DB.remove [ id ];
				return ()
	let list _ () =
		debug "VM.list";
		let module B = (val get_backend () : S) in
		let ls = List.fold_left (fun acc x ->
			let vm_t = x |> key_of |> DB.read |> unbox in
			let power = B.VM.get_power_state vm_t in
			(vm_t, power) :: acc
		) [] (DB.list []) in
		return ls

	let create _ id =
		debug "VM.create %s" id;
		let module B = (val get_backend () : S) in
		B.VM.create (id |> key_of |> DB.read |> unbox) |> return

	let build c id =
		debug "VM.build %s" id;
		let module B = (val get_backend () : S) in
		let vbds : Vbd.t list = VBD.list c id |> unwrap in
		let vifs : Vif.t list = VIF.list c id |> unwrap in
		B.VM.build (id |> key_of |> DB.read |> unbox) vbds vifs |> return

	let create_device_model c id =
		debug "VM.create_device_model %s" id;
		let module B = (val get_backend () : S) in
		B.VM.create_device_model (id |> key_of |> DB.read |> unbox) |> return

	let destroy _ id =
		debug "VM.destroy %s" id;
		let module B = (val get_backend () : S) in
		B.VM.destroy (id |> key_of |> DB.read |> unbox) |> return

	let pause _ id =
		debug "VM.pause %s" id;
		let module B = (val get_backend () : S) in
		B.VM.pause (id |> key_of |> DB.read |> unbox) |> return

	let unpause _ id =
		debug "VM.unpause %s" id;
		let module B = (val get_backend () : S) in
		B.VM.unpause (id |> key_of |> DB.read |> unbox) |> return

	let start c id =
		debug "VM.start %s" id;
		create c id |> unwrap;
		build c id |> unwrap;
		List.iter (fun vbd -> VBD.plug c vbd.Vbd.id |> unwrap) (VBD.list c id |> unwrap);
		List.iter (fun vif -> VIF.plug c vif.Vif.id |> unwrap) (VIF.list c id |> unwrap);
		(* Unfortunately this has to be done after the devices have been created since
		   qemu reads xenstore keys in preference to its own commandline. After this is
		   fixed we can consider creating qemu as a part of the 'build' *)
		create_device_model c id |> unwrap |> return

	let shutdown c id =
		debug "VM.shutdown %s" id;
		destroy c id |> unwrap;
		List.iter (fun vbd -> VBD.unplug c vbd.Vbd.id |> unwrap) (VBD.list c id |> unwrap);
		List.iter (fun vif -> VIF.unplug c vif.Vif.id |> unwrap) (VIF.list c id |> unwrap);
		return ()

	let suspend _ id disk =
		debug "VM.suspend %s" id;
		let module B = (val get_backend () : S) in
		B.VM.suspend (id |> key_of |> DB.read |> unbox) disk |> return

	let resume _ id disk =
		debug "VM.resume %s" id;
		let module B = (val get_backend () : S) in
		B.VM.resume (id |> key_of |> DB.read |> unbox) disk |> return
end


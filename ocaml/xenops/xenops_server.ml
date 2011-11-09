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
let get_backend () = match !backend with
  | Some x -> x 
  | None -> failwith "No backend implementation set"

let filter_prefix prefix xs =
	List.filter_map
		(fun x ->
			if String.startswith prefix x
			then Some (String.sub x (String.length prefix) (String.length x - (String.length prefix)))
			else None) xs

let updates = Updates.empty ()

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
	let plug' id =
		debug "VBD.plug %s" (string_of_id id);
		let module B = (val get_backend () : S) in	
	B.VBD.plug (vm_of id) (id |> key_of |> DB.read |> unbox)
	let plug _ id = plug' id |> return
	let unplug' id =
		debug "VBD.unplug %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		B.VBD.unplug (vm_of id) (id |> key_of |> DB.read |> unbox)
	let unplug _ id = unplug' id |> return
	let insert' id disk =
		debug "VBD.insert %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		let vbd_t = id |> key_of |> DB.read |> unbox in
		let state = B.VBD.get_state (vm_of id) vbd_t in
		if state.Vbd.media_present
		then raise (Exception Media_present)
		else begin
			B.VBD.insert (vm_of id) vbd_t disk;
			DB.write (key_of id) { vbd_t with Vbd.backend = Some disk };
		end
	let insert _ id disk = insert' id disk |> return
	let eject' id =
		debug "VBD.eject %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		let vbd_t = id |> key_of |> DB.read |> unbox in
		let state = B.VBD.get_state (vm_of id) vbd_t in
		if state.Vbd.media_present then begin
			B.VBD.eject (vm_of id) vbd_t;
			DB.write (key_of id) { vbd_t with Vbd.backend = None }
		end else raise (Exception Media_not_present)
	let eject _ id = eject' id |> return
	let remove _ id =
		debug "VBD.remove %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		if (B.VBD.get_state (vm_of id) (id |> key_of |> DB.read |> unbox)).Vbd.plugged
		then raise (Exception Device_is_connected)
		else return (DB.remove (key_of id))

	let stat' id =
		let module B = (val get_backend () : S) in
		let vbd_t = id |> key_of |> DB.read |> unbox in
		let state = B.VBD.get_state (vm_of id) vbd_t in
		vbd_t, state
	let stat _ id = return (stat' id)

	let list' vm =
		debug "VBD.list";
		let key_of' id = [ vm; "vbd." ^ id ] in
		let vbds = DB.list [ vm ] |> (filter_prefix "vbd.") |> (List.map (DB.read ++ key_of')) |> dropnone in
		let module B = (val get_backend () : S) in
		let states = List.map (B.VBD.get_state vm) vbds in
		List.combine vbds states

	let list _ vm = list' vm |> return
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
	let plug' id =
		debug "VIF.plug %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		B.VIF.plug (vm_of id) (id |> key_of |> DB.read |> unbox)
	let plug _ id = plug' id |> return
	let unplug' id =
		debug "VIF.unplug %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		B.VIF.unplug (vm_of id) (id |> key_of |> DB.read |> unbox)
	let unplug _ id = unplug' id |> return
	let remove _ id =
		debug "VIF.remove %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		if (B.VIF.get_state (vm_of id) (id |> key_of |> DB.read |> unbox)).Vif.plugged
		then raise (Exception Device_is_connected)
		else return (DB.remove (key_of id))

	let stat' id =
		let module B = (val get_backend () : S) in
		let vif_t = id |> key_of |> DB.read |> unbox in
		let state = B.VIF.get_state (vm_of id) vif_t in
		vif_t, state
	let stat _ id = return (stat' id)

	let list' vm =
		let key_of' id = [ vm; "vif." ^ id ] in
		let vifs = DB.list [ vm ] |> (filter_prefix "vif.") |> (List.map (DB.read ++ key_of')) |> dropnone in
		let module B = (val get_backend () : S) in
		let states = List.map (B.VIF.get_state vm) vifs in
		List.combine vifs states

	let list _ vm = list' vm |> return
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
		let power = (B.VM.get_state (id |> key_of |> DB.read |> unbox)).Vm.power_state in
		match power with
			| Running _ | Suspended | Paused -> raise (Exception (Bad_power_state(power, Halted)))
			| Halted ->
				DB.remove [ id ];
				return ()

	let stat' x =
		let module B = (val get_backend () : S) in
		let vm_t = x |> key_of |> DB.read |> unbox in
		let state = B.VM.get_state vm_t in
		vm_t, state
	let stat _ id = return (stat' id)

	let list _ () =
		debug "VM.list";
		let module B = (val get_backend () : S) in
		let ls = List.fold_left (fun acc x -> stat' x :: acc) [] (DB.list []) in
		return ls

	let create' id =
		debug "VM.create %s" id;
		let module B = (val get_backend () : S) in
		B.VM.create (id |> key_of |> DB.read |> unbox)

	let create _ id = create' id |> return

	let build' id =
		debug "VM.build %s" id;
		let module B = (val get_backend () : S) in
		let vbds : Vbd.t list = VBD.list' id |> List.map fst in
		let vifs : Vif.t list = VIF.list' id |> List.map fst in
		B.VM.build (id |> key_of |> DB.read |> unbox) vbds vifs

	let build _ id = build' id |> return

	let create_device_model' id =
		debug "VM.create_device_model %s" id;
		let module B = (val get_backend () : S) in
		B.VM.create_device_model (id |> key_of |> DB.read |> unbox)

	let create_device_model _ id = create_device_model' id |> return

	let destroy' id =
		debug "VM.destroy %s" id;
		let module B = (val get_backend () : S) in
		B.VM.destroy (id |> key_of |> DB.read |> unbox)

	let destroy _ id = destroy' id |> return

	let pause' id =
		debug "VM.pause %s" id;
		let module B = (val get_backend () : S) in
		B.VM.pause (id |> key_of |> DB.read |> unbox)

	let pause _ id = pause' id |> return

	let unpause' id =
		debug "VM.unpause %s" id;
		let module B = (val get_backend () : S) in
		B.VM.unpause (id |> key_of |> DB.read |> unbox)

	let unpause _ id = unpause' id |> return

	let start' id =
		debug "VM.start %s" id;
		create' id;
		build' id;
		List.iter (fun vbd -> VBD.plug' vbd.Vbd.id) (VBD.list' id |> List.map fst);
		List.iter (fun vif -> VIF.plug' vif.Vif.id) (VIF.list' id |> List.map fst);
		(* Unfortunately this has to be done after the devices have been created since
		   qemu reads xenstore keys in preference to its own commandline. After this is
		   fixed we can consider creating qemu as a part of the 'build' *)
		create_device_model' id

	let start _ id =
		start' id;
		Updates.add (Dynamic.Vm id) updates;
		return ()

	let shutdown' id =
		debug "VM.shutdown %s" id;
		destroy' id;
		List.iter (fun vbd -> VBD.unplug' vbd.Vbd.id) (VBD.list' id |> List.map fst);
		List.iter (fun vif -> VIF.unplug' vif.Vif.id) (VIF.list' id |> List.map fst)

	let shutdown _ id =
		shutdown' id;
		Updates.add (Dynamic.Vm id) updates;
		return ()

	let reboot' id =
		shutdown' id;
		start' id;
		unpause' id

	let shutdown_domain' id reason timeout =
		let module B = (val get_backend () : S) in
		let start = Unix.gettimeofday () in
		let vm = id |> key_of |> DB.read |> unbox in
		(* Spend at most the first minute waiting for a clean shutdown ack. This allows
		   us to abort early. *)
		if not (B.VM.request_shutdown vm reason (max 60. timeout))
		then raise (Exception Failed_to_acknowledge_shutdown_request);		
		let remaining_timeout = max 0. (timeout -. (Unix.gettimeofday () -. start)) in
		if not (B.VM.wait_shutdown vm reason remaining_timeout)
		then raise (Exception Failed_to_shutdown)

	let reboot _ id timeout =
		Opt.iter (shutdown_domain' id Reboot) timeout;				
		reboot' id;
		Updates.add (Dynamic.Vm id) updates;
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

module DEBUG = struct
	let trigger _ cmd args =
		let module B = (val get_backend () : S) in
		B.DEBUG.trigger cmd args |> return
end

module UPDATES = struct
	let lookup x =
		let module B = (val get_backend () : S) in
		try
			Some (match x with
				| Dynamic.Vm id -> let a, b = VM.stat' id in Dynamic.Vm_t (a, b)
				| Dynamic.Vbd id -> let a, b = VBD.stat' id in Dynamic.Vbd_t (a, b)
				| Dynamic.Vif id -> let a, b = VIF.stat' id in Dynamic.Vif_t (a, b)
			)
		with Exception Does_not_exist -> None

	let get _ last timeout =
		let ids, next = Updates.get last timeout updates in
		let ts = List.filter_map lookup ids in
		return (ts, next)
end

let internal_event_thread = ref None

let internal_event_thread_body () =
	debug "Starting internal event thread";
	let module B = (val get_backend () : S) in
	let id = ref None in
	while true do
		debug "About to call get with id = %s" (Opt.default "None" (Opt.map string_of_int !id));
		let updates, next_id = B.UPDATES.get !id None in
		debug "returned id = %s" (Opt.default "None" (Opt.map string_of_int next_id));
		assert (updates <> []);
		List.iter
			(function
				| Dynamic.Vm id, None ->
					debug "Ignoring event on unmanaged VM: %s" id
				| Dynamic.Vm id, Some (Dynamic.Vm_t (vm, state)) ->
					debug "Received an event on managed VM %s" vm.Vm.id;
					let actions = match B.VM.get_domain_action_request vm with
						| Some Needs_reboot -> vm.Vm.on_reboot
						| Some Needs_poweroff -> vm.Vm.on_shutdown
						| _ ->
							debug "Ignoring event on VM %s" vm.Vm.id;
							[] in
					(* There's an opportunity to schedule the actions *)
					List.iter
						(function
							| Vm.Coredump ->
								debug "Vm.Coredump unimplemented"
							| Vm.Shutdown ->
								VM.shutdown () vm.Vm.id |> unwrap
							| Vm.Start ->
								VM.reboot () vm.Vm.id None |> unwrap;
							| Vm.Delay ->
								debug "Vm.Delay unimplemented"
						) actions
				| id, _ ->
					debug "Ignoring event on %s" (Jsonrpc.to_string (Dynamic.rpc_of_id id))
			) (List.combine updates (List.map UPDATES.lookup updates));
		id := next_id
	done;
	debug "Shutting down internal event thread"

let set_backend m =
	backend := m;
	(* start the internal event thread *)
	internal_event_thread := Some (Thread.create internal_event_thread_body ());
	let module B = (val get_backend () : S) in
	B.init ()

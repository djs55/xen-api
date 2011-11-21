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
open Threadext
open Pervasiveext
open Fun
open Xenops_interface
open Xenops_server_plugin
open Xenops_utils

type context = {
	transferred_fd: Unix.file_descr option;
	(** some API calls take a file descriptor argument *)
}

let make_context () = {
	transferred_fd = None
}

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

type operation =
	| VM_start of Vm.id
	| VM_shutdown of Vm.id
	| VM_reboot of (Vm.id * float option)
	| VM_suspend of (Vm.id * disk)
	| VM_resume of (Vm.id * disk)
	| VM_restore of (Vm.id * disk)
	| VM_migrate of (Vm.id * string)
	| VM_shutdown_domain of (Vm.id * shutdown_request * float)
	| VM_destroy of Vm.id
	| VM_create of Vm.id
	| VM_build of Vm.id
	| VM_create_device_model of Vm.id
	| VM_pause of Vm.id
	| VM_unpause of Vm.id
	| VM_check_state of Vm.id
	| VBD_plug of Vbd.id
	| VBD_unplug of Vbd.id
	| VBD_insert of Vbd.id * disk
	| VBD_eject of Vbd.id
	| VIF_plug of Vif.id
	| VIF_unplug of Vif.id

module TASK = struct
	type t = {
		id: string;
		mutable result: Task.result;
		f: t -> unit;
		cancel: unit -> unit;
	}

	module SMap = Map.Make(struct type t = string let compare = compare end)

	let tasks = ref SMap.empty
	let m = Mutex.create ()
	let c = Condition.create ()

	let next_task_id =
		let counter = ref 0 in
		fun () ->
			let result = string_of_int !counter in
			incr counter;
			result

	let add (f: t -> unit) =
		let t = {
			id = next_task_id ();
			result = Task.Pending 0.;
			f = f;
			cancel = (fun () -> ());
		} in
		Mutex.execute m
			(fun () ->
				tasks := SMap.add t.id t !tasks
			);
		t

	let cancel _ id =
		Mutex.execute m
			(fun () ->
				if not (SMap.mem id !tasks) then raise (Exception Does_not_exist);
				let x = SMap.find id !tasks in
				x.cancel ()
			) |> return
	let stat' id =
		Mutex.execute m
			(fun () ->
				if not (SMap.mem id !tasks) then raise (Exception Does_not_exist);
				let x = SMap.find id !tasks in
				{
					Task.id = x.id;
					Task.result = x.result;
				}
			)
	let stat _ id = stat' id |> return
end

module Per_VM_queues = struct
	(* Single queue for now, one per Vm later *)
	let queue = Queue.create ()
	let m = Mutex.create ()
	let c = Condition.create ()

	let add vm f =
		Mutex.execute m
			(fun () ->
				Queue.push f queue;
				Condition.signal c)

	let rec process_queue q =
		let item =
			Mutex.execute m
				(fun () ->
					while Queue.is_empty q do
						Condition.wait c m
					done;
					Queue.pop q
				) in
		begin
			try
				item.TASK.f item;
				item.TASK.result <- Task.Completed;
				debug "Triggering event on task id %s" item.TASK.id;
				Updates.add (Dynamic.Task item.TASK.id) updates;
			with
				| Exception e ->
					debug "Caught exception while processing queue: %s" (e |> rpc_of_error |> Jsonrpc.to_string);
					item.TASK.result <- Task.Failed e;
					debug "Triggering event on task id %s" item.TASK.id;
					Updates.add (Dynamic.Task item.TASK.id) updates;
				| e ->
					debug "Caught exception while processing queue: %s" (Printexc.to_string e);
					item.TASK.result <- Task.Failed (Internal_error (Printexc.to_string e));
					debug "Triggering event on task id %s" item.TASK.id;
					Updates.add (Dynamic.Task item.TASK.id) updates;
		end;
		process_queue q

	let start () =
		let (_: Thread.t) = Thread.create process_queue queue in
		()
end

module VM_DB = struct
	include TypedTable(struct
		include Vm
		let namespace = "VM"
	end)
	let key_of id = [ id; "config" ]

	let list () =
		debug "VM.list";
		let module B = (val get_backend () : S) in
		let vms = List.map (fun x -> x |> key_of |> read |> unbox) (list []) in
		let states = List.map B.VM.get_state vms in
		List.combine vms states
end

module VBD_DB = struct
	include TypedTable(struct
		include Vbd
		let namespace = "VM"
	end)
	let key_of k = [ fst k; "vbd." ^ (snd k) ]
	let vm_of = fst
	let string_of_id (a, b) = a ^ "." ^ b

	let list vm =
		debug "VBD.list";
		let key_of' id = [ vm; "vbd." ^ id ] in
		let vbds = list [ vm ] |> (filter_prefix "vbd.") |> (List.map (read ++ key_of')) |> dropnone in
		let module B = (val get_backend () : S) in
		let states = List.map (B.VBD.get_state vm) vbds in
		List.combine vbds states
end

module VIF_DB = struct
	include TypedTable(struct
		include Vif
		let namespace = "VM"
	end)
	let key_of k = [ fst k; "vif." ^ (snd k) ]
	let vm_of = fst
	let string_of_id (a, b) = a ^ "." ^ b

	let list vm =
		let key_of' id = [ vm; "vif." ^ id ] in
		let vifs = list [ vm ] |> (filter_prefix "vif.") |> (List.map (read ++ key_of')) |> dropnone in
		let module B = (val get_backend () : S) in
		let states = List.map (B.VIF.get_state vm) vifs in
		List.combine vifs states
end

let rec perform (op: operation) (t: TASK.t) : unit =
	let module B = (val get_backend () : S) in
	match op with
		| VM_start id ->
			debug "VM.start %s" id;
			begin try
				perform (VM_create id) t;
				perform (VM_build id) t;
				List.iter (fun vbd -> perform (VBD_plug vbd.Vbd.id) t) (VBD_DB.list id |> List.map fst);
				List.iter (fun vif -> perform (VIF_plug vif.Vif.id) t) (VIF_DB.list id |> List.map fst);
				(* Unfortunately this has to be done after the devices have been created since
				   qemu reads xenstore keys in preference to its own commandline. After this is
				   fixed we can consider creating qemu as a part of the 'build' *)
				perform (VM_create_device_model id) t;
				Updates.add (Dynamic.Vm id) updates
			with e ->
				debug "VM.start threw error: %s. Calling VM.destroy" (Printexc.to_string e);
				perform (VM_destroy id) t;
				raise e
			end
		| VM_shutdown id ->
			debug "VM.shutdown %s" id;
			perform (VM_destroy id) t;
			List.iter (fun vbd -> perform (VBD_unplug vbd.Vbd.id) t) (VBD_DB.list id |> List.map fst);
			List.iter (fun vif -> perform (VIF_unplug vif.Vif.id) t) (VIF_DB.list id |> List.map fst);
			Updates.add (Dynamic.Vm id) updates
		| VM_reboot (id, timeout) ->
			debug "VM.reboot %s" id;
			Opt.iter (fun x -> perform (VM_shutdown_domain(id, Reboot, x)) t) timeout;
			perform (VM_shutdown id) t;
			perform (VM_start id) t;
			perform (VM_unpause id) t;
			Updates.add (Dynamic.Vm id) updates
		| VM_suspend (id, disk) ->
			debug "VM.suspend %s" id;
			B.VM.suspend (id |> VM_DB.key_of |> VM_DB.read |> unbox) disk;
			perform (VM_shutdown id) t;
			Updates.add (Dynamic.Vm id) updates
		| VM_restore (id, disk) ->
			debug "VM.restore %s" id;
			B.VM.restore (id |> VM_DB.key_of |> VM_DB.read |> unbox) disk
		| VM_resume (id, disk) ->
			debug "VM.resume %s" id;
			perform (VM_create id) t;
			perform (VM_restore (id, disk)) t;
			List.iter (fun vbd -> perform (VBD_plug vbd.Vbd.id) t) (VBD_DB.list id |> List.map fst);
			List.iter (fun vif -> perform (VIF_plug vif.Vif.id) t) (VIF_DB.list id |> List.map fst);
			(* Unfortunately this has to be done after the devices have been created since
			   qemu reads xenstore keys in preference to its own commandline. After this is
			   fixed we can consider creating qemu as a part of the 'build' *)
			perform (VM_create_device_model id) t;
			(* XXX: special flag? *)
			Updates.add (Dynamic.Vm id) updates
		| VM_migrate (id, url) ->
			debug "VM.migrate %s -> %s" id url;
			raise (Exception Unimplemented)
		| VM_shutdown_domain (id, reason, timeout) ->
			let start = Unix.gettimeofday () in
			let vm = id |> VM_DB.key_of |> VM_DB.read |> unbox in
			(* Spend at most the first minute waiting for a clean shutdown ack. This allows
			   us to abort early. *)
			if not (B.VM.request_shutdown vm reason (max 60. timeout))
			then raise (Exception Failed_to_acknowledge_shutdown_request);		
			let remaining_timeout = max 0. (timeout -. (Unix.gettimeofday () -. start)) in
			if not (B.VM.wait_shutdown vm reason remaining_timeout)
			then raise (Exception Failed_to_shutdown)
		| VM_destroy id ->
			debug "VM.destroy %s" id;
			B.VM.destroy (id |> VM_DB.key_of |> VM_DB.read |> unbox)
		| VM_create id ->
			debug "VM.create %s" id;
			B.VM.create (id |> VM_DB.key_of |> VM_DB.read |> unbox)
		| VM_build id ->
			debug "VM.build %s" id;
			let vbds : Vbd.t list = VBD_DB.list id |> List.map fst in
			let vifs : Vif.t list = VIF_DB.list id |> List.map fst in
			B.VM.build (id |> VM_DB.key_of |> VM_DB.read |> unbox) vbds vifs
		| VM_create_device_model id ->
			debug "VM.create_device_model %s" id;
			B.VM.create_device_model (id |> VM_DB.key_of |> VM_DB.read |> unbox)
		| VM_pause id ->
			debug "VM.pause %s" id;
			B.VM.pause (id |> VM_DB.key_of |> VM_DB.read |> unbox)
		| VM_unpause id ->
			debug "VM.unpause %s" id;
			B.VM.unpause (id |> VM_DB.key_of |> VM_DB.read |> unbox)
		| VM_check_state id ->
			let vm = id |> VM_DB.key_of |> VM_DB.read |> unbox in
			let actions = match B.VM.get_domain_action_request vm with
				| Some Needs_reboot -> vm.Vm.on_reboot
				| Some Needs_poweroff -> vm.Vm.on_shutdown
				| Some Needs_crashdump -> vm.Vm.on_crash
				| Some Needs_suspend ->
					debug "VM %s has unexpectedly suspended" id;
					[]
				| None ->
					debug "VM %s is not requesting any attention" id;
					[] in
			let operations_of_action = function
				| Vm.Coredump -> []
				| Vm.Shutdown -> [ VM_shutdown id ]
				| Vm.Start    -> [ VM_shutdown id; VM_start id; VM_unpause id ]
				| Vm.Delay    -> [] in
			let operations = List.concat (List.map operations_of_action actions) in
			List.iter (fun x -> perform x t) operations
		| VBD_plug id ->
			debug "VBD.plug %s" (VBD_DB.string_of_id id);
			B.VBD.plug (VBD_DB.vm_of id) (id |> VBD_DB.key_of |> VBD_DB.read |> unbox)
		| VBD_unplug id ->
			debug "VBD.unplug %s" (VBD_DB.string_of_id id);
			B.VBD.unplug (VBD_DB.vm_of id) (id |> VBD_DB.key_of |> VBD_DB.read |> unbox)
		| VBD_insert (id, disk) ->
			debug "VBD.insert %s" (VBD_DB.string_of_id id);
			let vbd_t = id |> VBD_DB.key_of |> VBD_DB.read |> unbox in
			let state = B.VBD.get_state (VBD_DB.vm_of id) vbd_t in
			if state.Vbd.media_present
			then raise (Exception Media_present)
			else begin
				B.VBD.insert (VBD_DB.vm_of id) vbd_t disk;
				VBD_DB.write (VBD_DB.key_of id) { vbd_t with Vbd.backend = Some disk };
			end
		| VBD_eject id ->
			debug "VBD.eject %s" (VBD_DB.string_of_id id);
			let module B = (val get_backend () : S) in
			let vbd_t = id |> VBD_DB.key_of |> VBD_DB.read |> unbox in
			let state = B.VBD.get_state (VBD_DB.vm_of id) vbd_t in
			if state.Vbd.media_present then begin
				B.VBD.eject (VBD_DB.vm_of id) vbd_t;
				VBD_DB.write (VBD_DB.key_of id) { vbd_t with Vbd.backend = None }
			end else raise (Exception Media_not_present)
		| VIF_plug id ->
			debug "VIF.plug %s" (VIF_DB.string_of_id id);
			B.VIF.plug (VIF_DB.vm_of id) (id |> VIF_DB.key_of |> VIF_DB.read |> unbox)
		| VIF_unplug id ->
			debug "VIF.unplug %s" (VIF_DB.string_of_id id);
			B.VIF.unplug (VIF_DB.vm_of id) (id |> VIF_DB.key_of |> VIF_DB.read |> unbox)

let queue_operation id op =
	let task = TASK.add (fun t -> perform op t) in
	Per_VM_queues.add id task;
	debug "Pushed task with id %s" task.TASK.id;
	task.TASK.id

module VBD = struct
	open Vbd
	module DB = VBD_DB

	let string_of_id (a, b) = a ^ "." ^ b
	let add _ x =
		debug "VBD.add %s %s" (string_of_id x.id) (Jsonrpc.to_string (rpc_of_t x));
		DB.add (DB.key_of x.id) x;
		return x.id

	let plug _ id = queue_operation (DB.vm_of id) (VBD_plug id) |> return
	let unplug _ id = queue_operation (DB.vm_of id) (VBD_unplug id) |> return

	let insert _ id disk = queue_operation (DB.vm_of id) (VBD_insert(id, disk)) |> return
	let eject _ id = queue_operation (DB.vm_of id) (VBD_eject id) |> return
	let remove _ id =
		debug "VBD.remove %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		if (B.VBD.get_state (DB.vm_of id) (id |> DB.key_of |> DB.read |> unbox)).Vbd.plugged
		then raise (Exception Device_is_connected)
		else return (DB.remove (DB.key_of id))

	let stat' id =
		let module B = (val get_backend () : S) in
		let vbd_t = id |> DB.key_of |> DB.read |> unbox in
		let state = B.VBD.get_state (DB.vm_of id) vbd_t in
		vbd_t, state
	let stat _ id = return (stat' id)

	let list _ vm = DB.list vm |> return
end

module VIF = struct
	open Vif

	module DB = VIF_DB

	let string_of_id (a, b) = a ^ "." ^ b
	let add _ x =
		debug "VIF.add %s" (Jsonrpc.to_string (rpc_of_t x));
		(* Generate MAC if necessary *)
		let mac = match x.mac with
			| "random" -> Device.Vif.random_local_mac ()
			| "" -> Device.Vif.hashchain_local_mac x.position (DB.vm_of x.id)
			| mac -> mac in
		DB.add (DB.key_of x.id) { x with mac = mac };
		return x.id

	let plug _ id = queue_operation (DB.vm_of id) (VIF_plug id) |> return
	let unplug _ id = queue_operation (DB.vm_of id) (VIF_unplug id) |> return

	let remove _ id =
		debug "VIF.remove %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		if (B.VIF.get_state (DB.vm_of id) (id |> DB.key_of |> DB.read |> unbox)).Vif.plugged
		then raise (Exception Device_is_connected)
		else return (DB.remove (DB.key_of id))

	let stat' id =
		let module B = (val get_backend () : S) in
		let vif_t = id |> DB.key_of |> DB.read |> unbox in
		let state = B.VIF.get_state (DB.vm_of id) vif_t in
		vif_t, state
	let stat _ id = return (stat' id)

	let list _ vm = DB.list vm |> return
end

module VM = struct
	open Vm

	module DB = VM_DB

	let add _ x =
		debug "VM.add %s" (Jsonrpc.to_string (rpc_of_t x));
		DB.add (DB.key_of x.id) x;
		return x.id
	let remove _ id =
		debug "VM.remove %s" id;
		let module B = (val get_backend () : S) in
		let power = (B.VM.get_state (id |> DB.key_of |> DB.read |> unbox)).Vm.power_state in
		match power with
			| Running _ | Suspended | Paused -> raise (Exception (Bad_power_state(power, Halted)))
			| Halted ->
				DB.remove [ id ];
				return ()

	let stat' x =
		let module B = (val get_backend () : S) in
		let vm_t = x |> DB.key_of |> DB.read |> unbox in
		let state = B.VM.get_state vm_t in
		vm_t, state
	let stat _ id = return (stat' id)

	let list _ () = DB.list () |> return

	let create _ id = queue_operation id (VM_create id) |> return

	let build _ id = queue_operation id (VM_build id) |> return

	let create_device_model _ id = queue_operation id (VM_create_device_model id) |> return

	let destroy _ id = queue_operation id (VM_destroy id) |> return

	let pause _ id = queue_operation id (VM_pause id) |> return

	let unpause _ id = queue_operation id (VM_unpause id) |> return

	let start _ id = queue_operation id (VM_start id) |> return

	let shutdown _ id = queue_operation id (VM_shutdown id) |> return

	let reboot _ id timeout = queue_operation id (VM_reboot (id, timeout)) |> return

	let suspend _ id disk = queue_operation id (VM_suspend (id, disk)) |> return

	let resume _ id disk = queue_operation id (VM_resume (id, disk)) |> return

	let migrate context id url = queue_operation id (VM_migrate (id, url)) |> return

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
				| Dynamic.Task id -> Dynamic.Task_t (TASK.stat' id)
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
					let (_: Task.id) = queue_operation vm.Vm.id (VM_check_state vm.Vm.id) in
					()
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

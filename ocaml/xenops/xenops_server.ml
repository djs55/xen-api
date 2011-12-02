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
open Xenops_task

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
	| VM_delay of (Vm.id * float) (** used to suppress fast reboot loops *)
	| VM_suspend of (Vm.id * data)
	| VM_resume of (Vm.id * data)
	| VM_save of (Vm.id * flag list * data)
	| VM_restore of (Vm.id * data)
	| VM_restore_devices of Vm.id
	| VM_migrate of (Vm.id * string)
	| VM_receive_memory of (Vm.id * Unix.file_descr)
	| VM_shutdown_domain of (Vm.id * shutdown_request * float)
	| VM_destroy of Vm.id
	| VM_create of Vm.id
	| VM_build of Vm.id
	| VM_create_device_model of (Vm.id * bool)
	| VM_pause of Vm.id
	| VM_unpause of Vm.id
	| VM_check_state of Vm.id
	| VM_remove of Vm.id
	| VBD_plug of Vbd.id
	| VBD_unplug of Vbd.id
	| VBD_insert of Vbd.id * disk
	| VBD_eject of Vbd.id
	| VIF_plug of Vif.id
	| VIF_unplug of Vif.id

module TASK = struct
	let cancel _ id =
		Mutex.execute m
			(fun () ->
				let x = find_locked id in
				x.cancel ()
			) |> return
	let stat' id =
		Mutex.execute m
			(fun () ->
				let x = find_locked id in
				{
					Task.id = x.id;
					Task.result = x.result;
					Task.subtasks = x.subtasks;
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
		Xenops_task.run item;
		debug "Triggering event on task id %s" item.Xenops_task.id;
		Updates.add (Dynamic.Task item.Xenops_task.id) updates;
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

module PCI_DB = struct
	include TypedTable(struct
		include Pci
		let namespace = "PCI"
	end)
	let key_of k = [ fst k; "pci." ^ (snd k) ]
	let vm_of = fst
	let string_of_id (a, b) = a ^ "." ^ b

	let list vm =
		debug "PCI.list";
		let key_of' id = [ vm; "pci." ^ id ] in
		let xs = list [ vm ] |> (filter_prefix "pci.") |> (List.map (read ++ key_of')) |> dropnone in
		let module B = (val get_backend () : S) in
		let states = List.map (B.PCI.get_state vm) xs in
		List.combine xs states
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

let export_metadata id =
	let module B = (val get_backend () : S) in
	let vm_t = id |> VM_DB.key_of |> VM_DB.read |> unbox in
	let vbds = VBD_DB.list id |> List.map fst in
	let vifs = VIF_DB.list id |> List.map fst in
	let domains = B.VM.get_internal_state vm_t in
	{
		Metadata.vm = vm_t;
		vbds = vbds;
		vifs = vifs;
		domains = domains;
	} |> Metadata.rpc_of_t |> Jsonrpc.to_string

let rec perform ?subtask (op: operation) (t: Xenops_task.t) : unit =
	let module B = (val get_backend () : S) in
	
	let one = function
		| VM_start id ->
			debug "VM.start %s" id;
			begin try
				perform ~subtask:"VM_create" (VM_create id) t;
				perform ~subtask:"VM_build" (VM_build id) t;
				List.iter (fun vbd -> perform ~subtask:(Printf.sprintf "VBD_plug %s" (snd vbd.Vbd.id)) (VBD_plug vbd.Vbd.id) t) (VBD_DB.list id |> List.map fst);
				List.iter (fun vif -> perform ~subtask:(Printf.sprintf "VIF_plug %s" (snd vif.Vif.id)) (VIF_plug vif.Vif.id) t) (VIF_DB.list id |> List.map fst);
				(* Unfortunately this has to be done after the devices have been created since
				   qemu reads xenstore keys in preference to its own commandline. After this is
				   fixed we can consider creating qemu as a part of the 'build' *)
				perform ~subtask:"VM_create_device_model" (VM_create_device_model (id, false)) t;
				Updates.add (Dynamic.Vm id) updates
			with e ->
				debug "VM.start threw error: %s. Calling VM.destroy" (Printexc.to_string e);
				perform ~subtask:"VM_destroy" (VM_destroy id) t;
				raise e
			end
		| VM_shutdown id ->
			debug "VM.shutdown %s" id;
			perform ~subtask:"VM_destroy" (VM_destroy id) t;
			List.iter (fun vbd -> perform ~subtask:(Printf.sprintf "VBD_unplug %s" (snd vbd.Vbd.id)) (VBD_unplug vbd.Vbd.id) t) (VBD_DB.list id |> List.map fst);
			List.iter (fun vif -> perform ~subtask:(Printf.sprintf "VIF_unplug %s" (snd vif.Vif.id)) (VIF_unplug vif.Vif.id) t) (VIF_DB.list id |> List.map fst);
			Updates.add (Dynamic.Vm id) updates
		| VM_reboot (id, timeout) ->
			debug "VM.reboot %s" id;
			Opt.iter (fun x -> perform ~subtask:"VM_shutdown_domain(Reboot)" (VM_shutdown_domain(id, Reboot, x)) t) timeout;
			perform ~subtask:"VM_shutdown" (VM_shutdown id) t;
			perform ~subtask:"VM_start" (VM_start id) t;
			perform ~subtask:"VM_unpause" (VM_unpause id) t;
			Updates.add (Dynamic.Vm id) updates
		| VM_delay (id, t) ->
			debug "VM %s: waiting for %.2f before next VM action" id t;
			Thread.delay t
		| VM_save (id, flags, data) ->
			debug "VM.save %s" id;
			B.VM.save t (id |> VM_DB.key_of |> VM_DB.read |> unbox) flags data
		| VM_restore (id, data) ->
			debug "VM.restore %s" id;
			B.VM.restore t (id |> VM_DB.key_of |> VM_DB.read |> unbox) data
		| VM_suspend (id, data) ->
			debug "VM.suspend %s" id;
			perform ~subtask:"VM_save" (VM_save (id, [], data)) t;
			perform ~subtask:"VM_shutdown" (VM_shutdown id) t;
			Updates.add (Dynamic.Vm id) updates
		| VM_restore_devices id -> (* XXX: this is delayed due to the 'attach'/'activate' behaviour *)
			debug "VM_restore_devices %s" id;
			List.iter (fun vbd -> perform ~subtask:(Printf.sprintf "VBD_plug %s" (snd vbd.Vbd.id)) (VBD_plug vbd.Vbd.id) t) (VBD_DB.list id |> List.map fst);
			List.iter (fun vif -> perform ~subtask:(Printf.sprintf "VIF_plug %s" (snd vif.Vif.id)) (VIF_plug vif.Vif.id) t) (VIF_DB.list id |> List.map fst);
			(* Unfortunately this has to be done after the devices have been created since
			   qemu reads xenstore keys in preference to its own commandline. After this is
			   fixed we can consider creating qemu as a part of the 'build' *)
			perform ~subtask:"VM_create_device_model" (VM_create_device_model (id, true)) t;
		| VM_resume (id, data) ->
			debug "VM.resume %s" id;
			perform ~subtask:"VM_create" (VM_create id) t;
			perform ~subtask:"VM_restore" (VM_restore (id, data)) t;
			perform ~subtask:"VM_restore_devices" (VM_restore_devices id) t;
			(* XXX: special flag? *)
			Updates.add (Dynamic.Vm id) updates
		| VM_migrate (id, url') ->
			debug "VM.migrate %s -> %s" id url';
			let open Xmlrpc_client in
			let open Xenops_client in
			let url = url' |> Http.Url.of_string in
			(* We need to perform version exchange here *)
			begin
				try
					debug "Remote system is: %s" (query url |> Query.rpc_of_t |> Jsonrpc.to_string)
				with e ->
					debug "Failed to contact remote system on %s: is it running? (%s)" url' (Printexc.to_string e);
					raise (Exception(Failed_to_contact_remote_service (url |> transport_of_url |> string_of_transport)))
			end;
			let module Remote = Xenops_interface.Client(struct let rpc = rpc url end) in
			let id = Remote.VM.import_metadata (export_metadata id) |> success in
			debug "Received id = %s" id;
			let suffix = Printf.sprintf "/memory/%s" id in
			let memory_url = match url with
				| Http.Url.Http(a, b) -> Http.Url.Http(a, b ^ suffix)
				| Http.Url.File(a, b) -> Http.Url.File(a, b ^ suffix) in
			with_transport (transport_of_url memory_url)
				(fun mfd ->
					Http_client.rpc mfd (Xenops_migrate.http_put memory_url)
						(fun response _ ->
							debug "XXX transmit memory";
							perform ~subtask:"memory transfer" (VM_save(id, [ Live ], FD mfd)) t;
							debug "XXX sending completed signal";
							Xenops_migrate.send_complete url id mfd;
							debug "XXX completed signal ok";
						)
				);
			perform ~subtask:"VM_shutdown" (VM_shutdown id) t;
			perform ~subtask:"VM_remove" (VM_remove id) t;
			Updates.add (Dynamic.Vm id) updates
		| VM_receive_memory (id, s) ->
			debug "VM.receive_memory %s" id;
			let state = B.VM.get_state (id |> VM_DB.key_of |> VM_DB.read |> unbox) in
			debug "VM.receive_memory %s power_state = %s" id (state.Vm.power_state |> rpc_of_power_state |> Jsonrpc.to_string);
			let response = Http.Response.make ~version:"1.1" "200" "OK" in
			response |> Http.Response.to_wire_string |> Unixext.really_write_string s;
			debug "VM.receive_memory calling create";
			perform ~subtask:"VM_create" (VM_create id) t;
			perform ~subtask:"VM_restore" (VM_restore(id, FD s)) t;
			debug "VM.receive_memory restore complete";
			(* Receive the all-clear to unpause *)
			(* We need to unmarshal the next HTTP request ourselves. *)
			Xenops_migrate.serve_rpc s
				(fun body ->
					let call = Jsonrpc.call_of_string body in
					let failure = Rpc.failure Rpc.Null in
					let success = Rpc.success (Xenops_migrate.Receiver.rpc_of_state Xenops_migrate.Receiver.Completed) in
					if call.Rpc.name = Xenops_migrate._complete then begin
						debug "Got VM.migrate_complete";
						perform ~subtask:"VM_restore_devices" (VM_restore_devices id) t;
						perform ~subtask:"VM_unpause" (VM_unpause id) t;
						success |> Jsonrpc.string_of_response
					end else begin
						debug "Something went wrong";
						perform ~subtask:"VM_shutdown" (VM_shutdown id) t;
						failure |> Jsonrpc.string_of_response
					end
				)
		| VM_shutdown_domain (id, reason, timeout) ->
			let start = Unix.gettimeofday () in
			let vm = id |> VM_DB.key_of |> VM_DB.read |> unbox in
			(* Spend at most the first minute waiting for a clean shutdown ack. This allows
			   us to abort early. *)
			if not (B.VM.request_shutdown t vm reason (max 60. timeout))
			then raise (Exception Failed_to_acknowledge_shutdown_request);		
			let remaining_timeout = max 0. (timeout -. (Unix.gettimeofday () -. start)) in
			if not (B.VM.wait_shutdown t vm reason remaining_timeout)
			then raise (Exception Failed_to_shutdown)
		| VM_destroy id ->
			debug "VM.destroy %s" id;
			B.VM.destroy t (id |> VM_DB.key_of |> VM_DB.read |> unbox)
		| VM_create id ->
			debug "VM.create %s" id;
			B.VM.create t (id |> VM_DB.key_of |> VM_DB.read |> unbox)
		| VM_build id ->
			debug "VM.build %s" id;
			let vbds : Vbd.t list = VBD_DB.list id |> List.map fst in
			let vifs : Vif.t list = VIF_DB.list id |> List.map fst in
			B.VM.build t (id |> VM_DB.key_of |> VM_DB.read |> unbox) vbds vifs
		| VM_create_device_model (id, save_state) ->
			debug "VM.create_device_model %s" id;
			B.VM.create_device_model t (id |> VM_DB.key_of |> VM_DB.read |> unbox) save_state
		| VM_pause id ->
			debug "VM.pause %s" id;
			B.VM.pause t (id |> VM_DB.key_of |> VM_DB.read |> unbox)
		| VM_unpause id ->
			debug "VM.unpause %s" id;
			B.VM.unpause t (id |> VM_DB.key_of |> VM_DB.read |> unbox)
		| VM_check_state id ->
			let vm = id |> VM_DB.key_of |> VM_DB.read |> unbox in
			let state = B.VM.get_state vm in
			let run_time = Unix.gettimeofday () -. state.Vm.last_start_time in
			debug "VM %s ran for %.2f seconds" id run_time;
			let actions = match B.VM.get_domain_action_request vm with
				| Some Needs_reboot -> vm.Vm.on_reboot
				| Some Needs_poweroff -> vm.Vm.on_shutdown
				| Some Needs_crashdump ->
					(* A VM which crashes too quickly should be shutdown *)
					if run_time < 120.0 then begin
						debug "VM %s crashed too quickly; shutting down" id;
						[ Vm.Shutdown ]
					end else vm.Vm.on_crash
				| Some Needs_suspend ->
					debug "VM %s has unexpectedly suspended" id;
					[]
				| None ->
					debug "VM %s is not requesting any attention" id;
					[] in
			let operations_of_action = function
				| Vm.Coredump -> []
				| Vm.Shutdown -> [ VM_shutdown id ]
				| Vm.Start    ->
					let delay = if run_time < 60. then begin
						debug "VM %s rebooted too quickly; inserting delay" id;
						[ VM_delay (id, 15.) ]
					end else [] in
					let restart = [ VM_shutdown id; VM_start id; VM_unpause id ] in
					delay @ restart
			in
			let operations = List.concat (List.map operations_of_action actions) in
			List.iter (fun x -> perform x t) operations
		| VM_remove id ->
			debug "VM.remove %s" id;
			let power = (B.VM.get_state (id |> VM_DB.key_of |> VM_DB.read |> unbox)).Vm.power_state in
			begin match power with
				| Running _ | Suspended | Paused -> raise (Exception (Bad_power_state(power, Halted)))
				| Halted ->
					VM_DB.remove [ id ]
			end
		| VBD_plug id ->
			debug "VBD.plug %s" (VBD_DB.string_of_id id);
			B.VBD.plug t (VBD_DB.vm_of id) (id |> VBD_DB.key_of |> VBD_DB.read |> unbox)
		| VBD_unplug id ->
			debug "VBD.unplug %s" (VBD_DB.string_of_id id);
			B.VBD.unplug t (VBD_DB.vm_of id) (id |> VBD_DB.key_of |> VBD_DB.read |> unbox)
		| VBD_insert (id, disk) ->
			debug "VBD.insert %s" (VBD_DB.string_of_id id);
			let vbd_t = id |> VBD_DB.key_of |> VBD_DB.read |> unbox in
			let vm_state = B.VM.get_state (VBD_DB.vm_of id |> VM_DB.key_of |> VM_DB.read |> unbox) in
			let vbd_state = B.VBD.get_state (VBD_DB.vm_of id) vbd_t in
			if vm_state.Vm.power_state = Running
			then
				if vbd_state.Vbd.media_present
				then raise (Exception Media_present)
				else B.VBD.insert t (VBD_DB.vm_of id) vbd_t disk;
			VBD_DB.write (VBD_DB.key_of id) { vbd_t with Vbd.backend = Some disk };
		| VBD_eject id ->
			debug "VBD.eject %s" (VBD_DB.string_of_id id);
			let vbd_t = id |> VBD_DB.key_of |> VBD_DB.read |> unbox in
			if vbd_t.Vbd.ty = Vbd.Disk then raise (Exception (Media_not_ejectable));
			let vm_state = B.VM.get_state (VBD_DB.vm_of id |> VM_DB.key_of |> VM_DB.read |> unbox) in
			let vbd_state = B.VBD.get_state (VBD_DB.vm_of id) vbd_t in
			if vm_state.Vm.power_state = Running
			then
				if vbd_state.Vbd.media_present
				then B.VBD.eject t (VBD_DB.vm_of id) vbd_t
				else raise (Exception Media_not_present);			
			VBD_DB.write (VBD_DB.key_of id) { vbd_t with Vbd.backend = None }
		| VIF_plug id ->
			debug "VIF.plug %s" (VIF_DB.string_of_id id);
			B.VIF.plug t (VIF_DB.vm_of id) (id |> VIF_DB.key_of |> VIF_DB.read |> unbox)
		| VIF_unplug id ->
			debug "VIF.unplug %s" (VIF_DB.string_of_id id);
			B.VIF.unplug t (VIF_DB.vm_of id) (id |> VIF_DB.key_of |> VIF_DB.read |> unbox)
	in
	match subtask with
		| None -> one op
		| Some name -> Xenops_task.with_subtask t name (fun () -> one op)

let queue_operation id op =
	let task = Xenops_task.add (fun t -> perform op t) in
	Per_VM_queues.add id task;
	debug "Pushed task with id %s" task.Xenops_task.id;
	task.Xenops_task.id

let immediate_operation id op =
	let task = Xenops_task.add (fun t -> perform op t) in
	Xenops_task.run task

module PCI = struct
	open Pci
	module DB = PCI_DB

	let string_of_id (a, b) = a ^ "." ^ b
	let add' x =
		debug "PCI.add %s %s" (string_of_id x.id) (Jsonrpc.to_string (rpc_of_t x));
		DB.add (DB.key_of x.id) x;
		x.id
	let add _ x = add' x |> return

	let remove _ id =
		debug "PCI.remove %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		if (B.PCI.get_state (DB.vm_of id) (id |> DB.key_of |> DB.read |> unbox)).Pci.plugged
		then raise (Exception Device_is_connected)
		else return (DB.remove (DB.key_of id))

	let list _ vm = DB.list vm |> return
end

module VBD = struct
	open Vbd
	module DB = VBD_DB

	let string_of_id (a, b) = a ^ "." ^ b
	let add' x =
		debug "VBD.add %s %s" (string_of_id x.id) (Jsonrpc.to_string (rpc_of_t x));
		DB.add (DB.key_of x.id) x;
		x.id
	let add _ x = add' x |> return

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
	let add' x =
		debug "VIF.add %s" (Jsonrpc.to_string (rpc_of_t x));
		(* Generate MAC if necessary *)
		let mac = match x.mac with
			| "random" -> Device.Vif.random_local_mac ()
			| "" -> Device.Vif.hashchain_local_mac x.position (DB.vm_of x.id)
			| mac -> mac in
		DB.add (DB.key_of x.id) { x with mac = mac };
		x.id
	let add _ x = add' x |> return

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

	let add' x =
		debug "VM.add %s" (Jsonrpc.to_string (rpc_of_t x));
		DB.add (DB.key_of x.id) x;
		x.id
	let add _ x = add' x |> return
	let remove _ id = immediate_operation id (VM_remove id) |> return

	let stat' x =
		let module B = (val get_backend () : S) in
		let vm_t = x |> DB.key_of |> DB.read |> unbox in
		let state = B.VM.get_state vm_t in
		vm_t, state
	let stat _ id = return (stat' id)

	let list _ () = DB.list () |> return

	let create _ id = queue_operation id (VM_create id) |> return

	let build _ id = queue_operation id (VM_build id) |> return

	let create_device_model _ id save_state = queue_operation id (VM_create_device_model (id, save_state)) |> return

	let destroy _ id = queue_operation id (VM_destroy id) |> return

	let pause _ id = queue_operation id (VM_pause id) |> return

	let unpause _ id = queue_operation id (VM_unpause id) |> return

	let start _ id = queue_operation id (VM_start id) |> return

	let shutdown _ id = queue_operation id (VM_shutdown id) |> return

	let reboot _ id timeout = queue_operation id (VM_reboot (id, timeout)) |> return

	let suspend _ id disk = queue_operation id (VM_suspend (id, Disk disk)) |> return

	let resume _ id disk = queue_operation id (VM_resume (id, Disk disk)) |> return

	let migrate context id url = queue_operation id (VM_migrate (id, url)) |> return

	let export_metadata _ id = export_metadata id |> return

	let import_metadata _ s =
		let module B = (val get_backend () : S) in
		let md = s |> Jsonrpc.of_string |> Metadata.t_of_rpc in
		let vm = add' md.Metadata.vm in
		let vbds = List.map (fun x -> { x with Vbd.id = (vm, snd x.Vbd.id) }) md.Metadata.vbds in
		let vifs = List.map (fun x -> { x with Vif.id = (vm, snd x.Vif.id) }) md.Metadata.vifs in
		let (_: Vbd.id list) = List.map VBD.add' vbds in
		let (_: Vif.id list) = List.map VIF.add' vifs in
		B.VM.set_internal_state (vm |> VM_DB.key_of |> VM_DB.read |> unbox) md.Metadata.domains;
		vm |> return

	let receive_memory req s _ =
		debug "VM.receive_memory";
		req.Http.Request.close <- true;
		(* The URI is /service/xenops/memory/id *)
		let bits = String.split '/' req.Http.Request.uri in
		let id = bits |> List.rev |> List.hd in
		debug "VM.receive_memory id = %s" id;
		immediate_operation id (VM_receive_memory(id, s))
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

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
(**
 * @group Storage
 *)

open OUnit

open Listext
open Stringext
open Pervasiveext
open Fun

open Xmlrpc_client

let default_path = "/var/xapi/xenopsd"
let transport = ref (Unix default_path)

let rpc call =
	XMLRPC_protocol.rpc ~transport:!transport
		~http:(xmlrpc ~version:"1.0" "/") call

open Xenops_interface

let usage_and_exit () =
	Printf.fprintf stderr "Usage:\n";
	Printf.fprintf stderr "  %s" Sys.argv.(0);
	exit 1

let success = function
	| (_, Some x) -> failwith (Jsonrpc.to_string (rpc_of_error x))
	| (Some x, _) -> x
	| None, None -> failwith "protocol error"

let fail_running = function
	| (_, Some (Bad_power_state(Running _, Halted))) -> ()
	| (_, Some x) -> failwith (Printf.sprintf "fail_running: %s" (Jsonrpc.to_string (rpc_of_error x)))
	| (Some x, _) -> failwith "expected failure, got success"
	| None, None -> failwith "protocol error"

let fail_not_built = function
	| (_, Some (Domain_not_built)) -> ()
	| (_, Some x) -> failwith (Printf.sprintf "fail_not_built: %s" (Jsonrpc.to_string (rpc_of_error x)))
	| (Some x, _) -> failwith "expected failure, got success"
	| None, None -> failwith "protocol error"

let fail_connected = function
	| (_, Some (Device_is_connected)) -> ()
	| (_, Some x) -> failwith (Jsonrpc.to_string (rpc_of_error x))
	| (Some x, _) -> failwith "expected failure, got success"
	| None, None -> failwith "protocol error"

let event_wait rpc p =
	let finished = ref false in
	let event_id = ref None in
	while not !finished do
		let deltas, next_id = Client.UPDATES.get rpc !event_id (Some 30) |> success in
		event_id := next_id;
		List.iter (fun d -> if p d then finished := true) deltas;
	done

let wait_for_task rpc id =
	Printf.fprintf stderr "wait_for id = %s\n%!" id;
	let finished = function
		| Dynamic.Task_t t ->
			Printf.fprintf stderr "got event for id %s\n%!" id;
			if t.Task.id = id then begin
				match t.Task.result with
				| Task.Pending _ -> false
				| Task.Completed -> true
				| Task.Failed _ -> true
			end else false
		| x ->
			Printf.fprintf stderr "ignore event on %s\n%!" (x |> Dynamic.rpc_of_t |> Jsonrpc.to_string);
			false in 
	event_wait rpc finished;
	id

let success_task rpc id =
	let t = Client.TASK.stat rpc id |> success in
	match t.Task.result with
	| Task.Completed -> ()
	| Task.Failed x -> failwith (Jsonrpc.to_string (rpc_of_error x))
	| Task.Pending _ -> failwith "task pending"

let fail_not_built_task rpc id =
	let t = Client.TASK.stat rpc id |> success in
	match t.Task.result with
	| Task.Completed -> failwith "task completed successfully: expected Domain_not_built"
	| Task.Failed Domain_not_built -> ()
	| Task.Failed x -> failwith (Jsonrpc.to_string (rpc_of_error x))
	| Task.Pending _ -> failwith "task pending"

let test_query _ = let (_: Query.t) = success (Client.query rpc ()) in ()

let missing_vm = "missing"

let vm_test_remove_missing _ =
	match Client.VM.remove rpc missing_vm with
		| Some _, _ -> failwith "VDI.remove succeeded"
		| None, Some Does_not_exist -> ()
		| _, _ -> failwith "protocol error"

let example_uuid = "c0ffeec0-ffee-c0ff-eec0-ffeec0ffeec0"

let ( ** ) = Int64.mul

let create_vm id =
	let open Vm in
	let _ = PV {
		framebuffer = false;
		Vm.boot = Indirect {
			bootloader = "pygrub";
			extra_args = "extra";
			legacy_args = "legacy";
			bootloader_args = "bootloader";
			devices = [ Local "0"; Local "1" ]
		}
	} in
	let hvm = HVM {
		hap = true;
		shadow_multiplier = 1.;
		timeoffset = "";
		video_mib = 4;
		video = Cirrus;
		acpi = true;
		serial = None;
		keymap = Some "en-gb";
		vnc_ip = Some "hello";
		pci_emulations = [ "1" ];
		pci_passthrough = false;
		boot_order = "boot";
		qemu_disk_cmdline = false;
	} in {
		id = id;
		name = "Example: " ^ id;
		ssidref = 1l;
		xsdata = [ "xs", "data" ];
		platformdata = [ "platform", "data" ];
		bios_strings = [ "bios", "strings" ];
		ty = hvm;
		suppress_spurious_page_faults = true;
		machine_address_size = None;
		memory_static_max = 128L ** 1024L ** 1024L;
		memory_dynamic_max = 128L ** 1024L ** 1024L;
		memory_dynamic_min = 128L ** 1024L ** 1024L;
		vcpus = 2;
		on_crash = [ Vm.Shutdown ];
		on_shutdown = [ Vm.Shutdown ];
		on_reboot = [ Vm.Start ];
	}

let sl x = Printf.sprintf "[ %s ]" (String.concat "; " (List.map (fun (k, v) -> k ^ ":" ^ v) x))

let vm_assert_equal vm vm' =
	let open Vm in
    assert_equal ~msg:"id" ~printer:(fun x -> x) vm.id vm'.id;
    assert_equal ~msg:"name" ~printer:(fun x -> x) vm.name vm'.name;
    assert_equal ~msg:"ssidref" ~printer:Int32.to_string vm.ssidref vm'.ssidref;
    assert_equal ~msg:"xsdata" ~printer:sl vm.xsdata vm'.xsdata;
    assert_equal ~msg:"platformdata" ~printer:sl vm.platformdata vm'.platformdata;
    assert_equal ~msg:"bios_strings" ~printer:sl vm.bios_strings vm'.bios_strings;
	assert_equal ~msg:"suppress_spurious_page_faults" ~printer:string_of_bool vm.suppress_spurious_page_faults vm'.suppress_spurious_page_faults;
	assert_equal ~msg:"machine_address_size" ~printer:(function None -> "None" | Some x -> string_of_int x) vm.machine_address_size vm'.machine_address_size;
	assert_equal ~msg:"memory_static_max" ~printer:Int64.to_string vm.memory_static_max vm'.memory_static_max;
	assert_equal ~msg:"memory_dynamic_max" ~printer:Int64.to_string vm.memory_dynamic_max vm'.memory_dynamic_max;
	assert_equal ~msg:"memory_dynamic_min" ~printer:Int64.to_string vm.memory_dynamic_min vm'.memory_dynamic_min;
	assert_equal ~msg:"vcpus" ~printer:string_of_int vm.vcpus vm'.vcpus;
	assert_equal ~msg:"on_crash" ~printer:(fun x -> String.concat ", " (List.map (fun x -> x |> Vm.rpc_of_action |> Jsonrpc.to_string) x)) vm.on_crash vm'.on_crash;
	assert_equal ~msg:"on_shutdown" ~printer:(fun x -> String.concat ", " (List.map (fun x -> x |> Vm.rpc_of_action |> Jsonrpc.to_string) x)) vm.on_shutdown vm'.on_shutdown;
	assert_equal ~msg:"on_reboot" ~printer:(fun x -> String.concat ", " (List.map (fun x -> x |> Vm.rpc_of_action |> Jsonrpc.to_string) x)) vm.on_reboot vm'.on_reboot;
	let is_hvm vm = match vm.ty with
		| HVM _ -> true | PV _ -> false in
	assert_equal ~msg:"HVM-ness" ~printer:string_of_bool (is_hvm vm) (is_hvm vm');
	match vm.ty, vm'.ty with
		| HVM _, PV _
		| PV _, HVM _ -> failwith "HVM-ness"
		| HVM h, HVM h' ->
			assert_equal ~msg:"HAP" ~printer:string_of_bool h.hap h'.hap;
			assert_equal ~msg:"shadow_multipler" ~printer:string_of_float h.shadow_multiplier h'.shadow_multiplier;
			assert_equal ~msg:"timeoffset" ~printer:(fun x -> x) h.timeoffset h'.timeoffset;
			assert_equal ~msg:"video_mib" ~printer:string_of_int h.video_mib h'.video_mib;
			assert_equal ~msg:"video" ~printer:(fun x -> x |> rpc_of_video_card |> Jsonrpc.to_string) h.video h'.video;
			assert_equal ~msg:"acpi" ~printer:string_of_bool h.acpi h'.acpi;
			assert_equal ~msg:"serial" ~printer:(Opt.default "None") h.serial h'.serial;
			assert_equal ~msg:"keymap" ~printer:(Opt.default "None") h.keymap h'.keymap;
			assert_equal ~msg:"vnc_ip" ~printer:(Opt.default "None") h.vnc_ip h'.vnc_ip;
			assert_equal ~msg:"pci_emulations" ~printer:(String.concat ";")  h.pci_emulations h'.pci_emulations;
			assert_equal ~msg:"pci_passthrough" ~printer:string_of_bool  h.pci_passthrough h'.pci_passthrough;
			assert_equal ~msg:"boot_order" ~printer:(fun x -> x) h.boot_order h'.boot_order;
			assert_equal ~msg:"qemu_disk_cmdline" ~printer:string_of_bool h.qemu_disk_cmdline h'.qemu_disk_cmdline;
		| PV p, PV p' ->
			assert_equal ~msg:"framebuffer" ~printer:string_of_bool p.framebuffer p'.framebuffer;
			begin match p.boot, p'.boot with
				| Direct _, Indirect _
				| Indirect _, Direct _ -> failwith "pv-boot-ness"
				| Direct x, Direct x' ->
					assert_equal ~msg:"kernel" ~printer:(fun x -> x) x.kernel x'.kernel;
					assert_equal ~msg:"cmdline" ~printer:(fun x -> x) x.cmdline x'.cmdline;
					assert_equal ~msg:"ramdisk" ~printer:(function None -> "None" | Some x -> x) x.ramdisk x'.ramdisk		
				| Indirect x, Indirect x' ->
					assert_equal ~msg:"bootloader" ~printer:(fun x -> x) x.bootloader x'.bootloader;
					assert_equal ~msg:"extra_args" ~printer:(fun x -> x) x.extra_args x'.extra_args;
					assert_equal ~msg:"legacy_args" ~printer:(fun x -> x) x.legacy_args x'.legacy_args;
					assert_equal ~msg:"bootloader_args" ~printer:(fun x -> x) x.bootloader_args x'.bootloader_args;
					assert_equal ~msg:"devices" ~printer:(fun x -> x |> rpc_of_disk_list |> Jsonrpc.to_string) x.devices x'.devices;
			end

let with_vm id f =
	let vm = create_vm id in
	let (id: Vm.id) = success (Client.VM.add rpc vm) in
	finally (fun () -> f id)
		(fun () ->
			try
				success (Client.VM.remove rpc id)
			with e ->
				Printf.fprintf stderr "Caught failure during with_vm cleanup: %s" (Printexc.to_string e);
				raise e
		)

let vm_test_add_remove _ =
	with_vm example_uuid (fun _ -> ())


let vm_test_create_destroy _ =
	with_vm example_uuid
		(fun id ->
			Client.VM.create rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.destroy rpc id |> success |> wait_for_task rpc |> success_task rpc;
		)

let vm_test_pause_unpause _ =
	with_vm example_uuid
		(fun id ->
			Client.VM.create rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.unpause rpc id |> success |> wait_for_task rpc |> fail_not_built_task rpc;
			Client.VM.pause rpc id |> success |> wait_for_task rpc |> fail_not_built_task rpc;
			Client.VM.destroy rpc id |> success |> wait_for_task rpc |> success_task rpc;
		)

let vm_test_build_pause_unpause _ =
	with_vm example_uuid
		(fun id ->
			Client.VM.create rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.build rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.unpause rpc id |> success |> wait_for_task rpc |> fail_not_built_task rpc;
			Client.VM.create_device_model rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.unpause rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.pause rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.destroy rpc id |> success |> wait_for_task rpc |> success_task rpc;
		)

let vm_test_add_list_remove _ =
	with_vm example_uuid
		(fun id ->
			let vm = create_vm example_uuid in
			let (vms: (Vm.t * Vm.state) list) = success (Client.VM.list rpc ()) in
			let vm' = List.find (fun x -> x.Vm.id = id) (List.map fst vms) in
			vm_assert_equal vm vm'
		)

let vm_remove_running _ =
	with_vm example_uuid
		(fun id ->
			Client.VM.create rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.build rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.create_device_model rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.unpause rpc id |> success |> wait_for_task rpc |> success_task rpc;
			fail_running (Client.VM.remove rpc id);
			Client.VM.destroy rpc id |> success |> wait_for_task rpc |> success_task rpc;
		)

let vm_test_start_shutdown _ =
	with_vm example_uuid
		(fun id ->
			Client.VM.start rpc id |> success |> wait_for_task rpc |> success_task rpc;
			fail_running (Client.VM.remove rpc id);
			Client.VM.shutdown rpc id |> success |> wait_for_task rpc |> success_task rpc;
		)

let vm_test_consoles _ =
	()
(*
	with_vm example_uuid
		(fun id ->
			success (Client.VM.start rpc id);
			let (_: Console.t list) = success (Client.CONSOLE.list rpc id) in
			success (Client.VM.shutdown rpc id);
		)
*)

let vm_test_reboot _ =
	with_vm example_uuid
		(fun id ->
			Client.VM.create rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.build rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.create_device_model rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.unpause rpc id |> success |> wait_for_task rpc |> success_task rpc;
			let state : Vm.state = Client.VM.stat rpc id |> success |> snd in
			success (Client.DEBUG.trigger rpc "reboot" [ id ]);
			(* ... need to wait for the domain id to change *)
			event_wait rpc
				(function
					| Dynamic.Vm_t (vm_t, vm_state) ->
						vm_t.Vm.id = id && vm_state.Vm.domids <> state.Vm.domids
					| _ -> false);
			Client.VM.shutdown rpc id |> success |> wait_for_task rpc |> success_task rpc;
		)

let vm_test_halt _ =
	with_vm example_uuid
		(fun id ->
			Client.VM.create rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.build rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.create_device_model rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.unpause rpc id |> success |> wait_for_task rpc |> success_task rpc;
			success (Client.DEBUG.trigger rpc "halt" [ id ]);
			(* ... need to wait for the domain ids to disappear *)
			event_wait rpc
				(function
					| Dynamic.Vm_t (vm_t, vm_state) ->
						vm_t.Vm.id = id && vm_state.Vm.domids = []
					| _ -> false);
			Client.VM.shutdown rpc id |> success |> wait_for_task rpc |> success_task rpc;
		)

let vm_test_suspend _ =
	with_vm example_uuid
		(fun id ->
			Client.VM.create rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.build rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.unpause rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.suspend rpc id (Local "disk") |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.destroy rpc id |> success |> wait_for_task rpc |> success_task rpc;
		)

let vm_test_resume _ =
	with_vm example_uuid
		(fun id ->
			Client.VM.create rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.resume rpc id (Local "disk") |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.unpause rpc id |> success |> wait_for_task rpc |> success_task rpc;
			Client.VM.destroy rpc id |> success |> wait_for_task rpc |> success_task rpc;
		)
	

module type DEVICE = sig
	type t
	type state
	val assert_equal: t -> t -> unit
	type position
	val positions: position list
	type id
	val ids: id list
	val create: id -> position -> t
	val add: t -> id option * error option
	val remove: id -> unit option * error option
	val plug: id -> Task.id option * error option
	val unplug: id -> Task.id option * error option
	val list: Vm.id -> (t * state) list option * error option
	val find: id -> (t * state) list -> t
end

module DeviceTests = functor(D: DEVICE) -> struct
	open D
	let add_remove _ =
		with_vm example_uuid
			(fun id ->
				let dev = create (List.hd ids) (List.hd positions) in
				let (dev_id: id) = success (add dev) in
				success (remove dev_id)
			)

	let with_added_vm id f =
		with_vm id
			(fun id ->
				Client.VM.create rpc id |> success |> wait_for_task rpc |> success_task rpc;
				finally
					(fun () -> f id)
					(fun () -> 
						Client.VM.destroy rpc id |> success |> wait_for_task rpc |> success_task rpc
					)
			)

	let add_plug_unplug_remove _ =
		with_added_vm example_uuid
			(fun id ->
				let dev = create (List.hd ids) (List.hd positions) in
				let (dev_id: id) = success (add dev) in
				plug dev_id |> success |> wait_for_task rpc |> success_task rpc;
				unplug dev_id |> success |> wait_for_task rpc |> success_task rpc;
				success (remove dev_id);
			)

	let add_plug_unplug_many_remove _ =
		with_added_vm example_uuid
			(fun id ->
				let ids = 
					List.map
						(fun (id, position) ->
							let dev = create id position in
							let id = success (add dev) in
							plug id |> success |> wait_for_task rpc |> success_task rpc;
							id
						) (List.combine ids positions) in
				List.iter
					(fun id ->
						unplug id |> success |> wait_for_task rpc |> success_task rpc;
						success (remove id);
					) ids
			)

	let add_list_remove _ =
		with_vm example_uuid
			(fun id ->
				let dev = create (List.hd ids) (List.hd positions) in
				let (dev_id: id) = success (add dev) in
				let (devs: (t * state) list) = success (list id) in
				let dev' = find dev_id devs in
				assert_equal dev dev';
				success (remove dev_id);
			)

	let add_vm_remove _ =
		with_vm example_uuid
			(fun id ->
				let dev = create (List.hd ids) (List.hd positions) in
				let (_: id) = success (add dev) in
				()
			)

	let remove_running _ =
		with_added_vm example_uuid
			(fun id ->
				let dev = create (List.hd ids) (List.hd positions) in
				let (dev_id: id) = success (add dev) in
				plug dev_id |> success |> wait_for_task rpc |> success_task rpc;
				(* no unplug *)
				fail_connected (remove dev_id);				
			)
end

module VbdDeviceTests = DeviceTests(struct
	type t = Vbd.t
	type id = Vbd.id
	type state = Vbd.state
	type position = Device_number.t option
	let positions = [ None; None; None ]
	let ids = List.map (fun x -> example_uuid, x) [ "0"; "1"; "2" ]
	let create id position =
		let open Vbd in {
			Vbd.id = id;
			position = position;
			mode = ReadWrite;
			backend = Some (Local "/dev/zero");
			ty = Disk;
			unpluggable = true;
			extra_backend_keys = [ "backend", "keys" ];
			extra_private_keys = [ "private", "keys" ];
		}
	let add = Client.VBD.add rpc
	let remove = Client.VBD.remove rpc
	let plug = Client.VBD.plug rpc
	let unplug = Client.VBD.unplug rpc
	let list = Client.VBD.list rpc
	let find id vbds = List.find (fun (x, _) -> x.Vbd.id = id) vbds |> fst
	let assert_equal vbd vbd' =
		let open Vbd in
		assert_equal ~msg:"id" ~printer:(fun (a, b) -> Printf.sprintf "%s.%s" a b) vbd.id vbd'.id;
		assert_equal ~msg:"mode" ~printer:(function ReadWrite -> "RW" | ReadOnly -> "RO") vbd.mode vbd'.mode;
		assert_equal ~msg:"backend" ~printer:(fun x -> Opt.default "None" (Opt.map (fun x -> x |> rpc_of_disk |> Jsonrpc.to_string) x)) vbd.backend vbd'.backend;
		assert_equal ~msg:"unpluggable" ~printer:string_of_bool vbd.unpluggable vbd'.unpluggable;
		assert_equal ~msg:"extra_backend_keys" ~printer:sl vbd.extra_backend_keys vbd'.extra_backend_keys;
		assert_equal ~msg:"extra_private_keys" ~printer:sl vbd.extra_private_keys vbd'.extra_private_keys
end)

module VifDeviceTests = DeviceTests(struct
	type t = Vif.t
	type id = Vif.id
	type state = Vif.state
	type position = int
	let positions = [ 0; 1; 2 ]
	let ids = List.map (fun x -> example_uuid, x) [ "0"; "1"; "2" ]
	let create id position =
		let open Vif in {
			id = id;
			position = position;
			mac = "c0:ff:ee:c0:ff:ee";
			carrier = false;
			mtu = 1450;
			rate = Some(1L, 2L);
			backend = Bridge "xenbr0";
			other_config = [ "other", "config" ];
			extra_private_keys = [ "private", "keys" ];
		}
	let add = Client.VIF.add rpc
	let remove = Client.VIF.remove rpc
	let plug = Client.VIF.plug rpc
	let unplug = Client.VIF.unplug rpc
	let list = Client.VIF.list rpc
	let find id vifs = List.find (fun (x, _) -> x.Vif.id = id) vifs |> fst
	let assert_equal vif vif' =
		let open Vif in
		assert_equal ~msg:"id" ~printer:(fun (a, b) -> Printf.sprintf "%s.%s" a b) vif.id vif'.id;
		assert_equal ~msg:"position" ~printer:string_of_int vif.position vif'.position;
		assert_equal ~msg:"mac" ~printer:(fun x -> x) vif.mac vif'.mac;
		assert_equal ~msg:"carrier" ~printer:string_of_bool vif.carrier vif'.carrier;
		assert_equal ~msg:"mtu" ~printer:string_of_int vif.mtu vif'.mtu;
		assert_equal ~msg:"rate" ~printer:(function Some (a, b) -> Printf.sprintf "Some %Ld %Ld" a b | None -> "None") vif.rate vif'.rate;
		assert_equal ~msg:"backend" ~printer:(fun x -> x |> rpc_of_network |> Jsonrpc.to_string) vif.backend vif'.backend;
		assert_equal ~msg:"other_config" ~printer:sl vif.other_config vif'.other_config;
		assert_equal ~msg:"extra_private_keys" ~printer:sl vif.extra_private_keys vif'.extra_private_keys
end)


let _ =
	let verbose = ref false in
	let unix_path = ref "" in
	let host_port = ref "" in

	Arg.parse [
		"-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
		"-tcp", Arg.Set_string host_port, "Connect via TCP to a host:port";
		"-unix", Arg.Set_string unix_path, Printf.sprintf "Connect via a Unix domain socket (default %s)" default_path
	] (fun x -> Printf.fprintf stderr "Ignoring argument: %s\n" x)
		"Test via storage backend";

	if !host_port <> "" && (!unix_path <> "") then failwith "Please supply either -tcp OR -unix";

	if !host_port <> "" then begin
		match String.split ~limit:2 ':' !host_port with
			| [ host; port ] ->
				transport := TCP(host, int_of_string port)
			| _ -> failwith "Failed to parse host:port"
	end;
	if !unix_path <> "" then transport := Unix(!unix_path);

	let suite = "xenops test" >::: 
		[
			"test_query" >:: test_query;
			"vm_test_remove_missing" >:: vm_test_remove_missing;
			"vm_test_add_remove" >:: vm_test_add_remove;
			"vm_test_create_destroy" >:: vm_test_create_destroy;
			"vm_test_pause_unpause" >:: vm_test_pause_unpause;
			"vm_test_build_pause_unpause" >:: vm_test_build_pause_unpause;
			"vm_test_add_list_remove" >:: vm_test_add_list_remove;
			"vm_remove_running" >:: vm_remove_running;
			"vm_test_start_shutdown" >:: vm_test_start_shutdown;
			"vm_test_consoles" >:: vm_test_consoles;
			"vm_test_reboot" >:: vm_test_reboot;
			"vm_test_halt" >:: vm_test_halt;
			"vbd_test_add_remove" >:: VbdDeviceTests.add_remove;
			"vbd_test_add_list_remove" >:: VbdDeviceTests.add_list_remove;
			"vbd_test_add_vm_remove" >:: VbdDeviceTests.add_vm_remove;
			"vbd_test_add_plug_unplug_remove" >:: VbdDeviceTests.add_plug_unplug_remove;
			"vbd_test_add_plug_unplug_many_remove" >:: VbdDeviceTests.add_plug_unplug_many_remove;
			"vbd_remove_running" >:: VbdDeviceTests.remove_running;
			"vif_test_add_remove" >:: VifDeviceTests.add_remove;
			"vif_test_add_list_remove" >:: VifDeviceTests.add_list_remove;
			"vif_test_add_vm_remove" >:: VifDeviceTests.add_vm_remove;
			"vif_test_add_plug_unplug_remove" >:: VifDeviceTests.add_plug_unplug_remove;
			"vif_test_add_plug_unplug_many_remove" >:: VifDeviceTests.add_plug_unplug_many_remove;
			"vif_remove_running" >:: VifDeviceTests.remove_running;
(*
			"vm_test_suspend" >:: vm_test_suspend;
			"vm_test_resume" >:: vm_test_resume;
*)
		] in

	run_test_tt ~verbose:!verbose suite


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
	| (_, Some (Bad_power_state(Running, Halted))) -> ()
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

let test_query _ = let (_: Query.t) = success (Client.query rpc ()) in ()

let missing_vm = "missing"

let vm_test_destroy_missing _ =
	match Client.VM.destroy rpc missing_vm with
		| Some _, _ -> failwith "VDI.destroy succeeded"
		| None, Some Does_not_exist -> ()
		| _, _ -> failwith "protocol error"

let example_uuid = "c0ffeec0-ffee-c0ff-eec0-ffeec0ffeec0"

let ( ** ) = Int64.mul

let make_vm id =
	let open Vm in
	let _ = PV {
		boot = Indirect {
			bootloader = "pygrub";
			extra_args = "extra";
			legacy_args = "legacy";
			bootloader_args = "bootloader";
			devices = [ ("dom0", "0"); ("dom0", "1") ]
		}
	} in
	let hvm = HVM {
		hap = true;
		shadow_multiplier = 1.;
		timeoffset = "";
		video_mib = 4;
	} in {
		id = id;
		domid = None;
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
	}

let sl x = Printf.sprintf "[ %s ]" (String.concat "; " (List.map (fun (k, v) -> k ^ ":" ^ v) x))

let vm_assert_equal vm vm' =
	let open Vm in
    assert_equal ~msg:"id" ~printer:(fun x -> x) vm.id vm'.id;
    assert_equal ~msg:"domid" ~printer:(function None -> "None" | Some x -> string_of_int x) vm.domid vm'.domid;
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
			assert_equal ~msg:"video_mib" ~printer:string_of_int h.video_mib h'.video_mib
		| PV p, PV p' ->
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
					assert_equal ~msg:"devices" ~printer:(fun xs -> String.concat "; " (List.map (fun (x, y) -> x ^ ":" ^ y) xs)) x.devices x'.devices;
			end

let with_vm id f =
	let vm = make_vm id in
	let (id: Vm.id) = success (Client.VM.create rpc vm) in
	finally (fun () -> f id)
		(fun () ->
			try
				success (Client.VM.destroy rpc id)
			with e ->
				Printf.fprintf stderr "Caught failure during with_vm cleanup: %s" (Printexc.to_string e);
				raise e
		)

let vm_test_create_destroy _ =
	with_vm example_uuid (fun _ -> ())


let vm_test_make_shutdown _ =
	with_vm example_uuid
		(fun id ->
			success (Client.VM.make rpc id);
			success (Client.VM.shutdown rpc id)
		)

let vm_test_pause_unpause _ =
	with_vm example_uuid
		(fun id ->
			success (Client.VM.make rpc id);
			fail_not_built (Client.VM.unpause rpc id);
			fail_not_built (Client.VM.pause rpc id);
			success (Client.VM.shutdown rpc id)
		)

let vm_test_build_pause_unpause _ =
	with_vm example_uuid
		(fun id ->
			success (Client.VM.make rpc id);
			success (Client.VM.build rpc id);
			success (Client.VM.unpause rpc id);
			success (Client.VM.pause rpc id);
			success (Client.VM.shutdown rpc id);
		)

let vm_test_create_list_destroy _ =
	with_vm example_uuid
		(fun id ->
			let vm = make_vm example_uuid in
			let (vms: Vm.t list) = success (Client.VM.list rpc ()) in
			let vm' = List.find (fun x -> x.Vm.id = id) vms in
			vm_assert_equal vm vm'
		)

let vm_destroy_running _ =
	with_vm example_uuid
		(fun id ->
			success (Client.VM.make rpc id);
			success (Client.VM.build rpc id);
			success (Client.VM.unpause rpc id);
			fail_running (Client.VM.destroy rpc id);
			success (Client.VM.shutdown rpc id)
		)

let vm_test_suspend _ =
	with_vm example_uuid
		(fun id ->
			success (Client.VM.make rpc id);
			success (Client.VM.build rpc id);
			success (Client.VM.unpause rpc id);
			success (Client.VM.suspend rpc id ("some", "disk"));
			success (Client.VM.shutdown rpc id)
		)

let vm_test_resume _ =
	with_vm example_uuid
		(fun id ->
			success (Client.VM.make rpc id);
			success (Client.VM.resume rpc id ("some", "disk"));
			success (Client.VM.unpause rpc id);
			success (Client.VM.shutdown rpc id)
		)
	

module type DEVICE = sig
	type t
	val assert_equal: t -> t -> unit
	type position
	val positions: position list
	type id
	val ids: id list
	val make: id -> position -> t
	val create: t -> id option * error option
	val destroy: id -> unit option * error option
	val plug: id -> unit option * error option
	val unplug: id -> unit option * error option
	val list: Vm.id -> t list option * error option
	val find: id -> t list -> t
end

module DeviceTests = functor(D: DEVICE) -> struct
	open D
	let create_destroy _ =
		with_vm example_uuid
			(fun id ->
				let dev = make (List.hd ids) (List.hd positions) in
				let (dev_id: id) = success (create dev) in
				success (destroy dev_id)
			)

	let with_created_vm id f =
		with_vm id
			(fun id ->
				success (Client.VM.make rpc id);
				finally
					(fun () -> f id)
					(fun () -> success (Client.VM.shutdown rpc id))
			)

	let create_plug_unplug_destroy _ =
		with_created_vm example_uuid
			(fun id ->
				let dev = make (List.hd ids) (List.hd positions) in
				let (dev_id: id) = success (create dev) in
				success (plug dev_id);
				success (unplug dev_id);
				success (destroy dev_id);
			)

	let create_plug_unplug_many_destroy _ =
		with_created_vm example_uuid
			(fun id ->
				let ids = 
					List.map
						(fun (id, position) ->
							let dev = make id position in
							let id = success (create dev) in
							success (plug id);
							id
						) (List.combine ids positions) in
				List.iter
					(fun id ->
						success (unplug id);
						success (destroy id);
					) ids
			)

	let create_list_destroy _ =
		with_vm example_uuid
			(fun id ->
				let dev = make (List.hd ids) (List.hd positions) in
				let (dev_id: id) = success (create dev) in
				let (devs: t list) = success (list id) in
				let dev' = find dev_id devs in
				assert_equal dev dev';
				success (destroy dev_id);
			)

	let create_vm_destroy _ =
		with_vm example_uuid
			(fun id ->
				let dev = make (List.hd ids) (List.hd positions) in
				let (_: id) = success (create dev) in
				()
			)

	let destroy_running _ =
		with_created_vm example_uuid
			(fun id ->
				let dev = make (List.hd ids) (List.hd positions) in
				let (dev_id: id) = success (create dev) in
				success (plug dev_id);
				(* no unplug *)
				fail_connected (destroy dev_id);				
			)
end

module VbdDeviceTests = DeviceTests(struct
	type t = Vbd.t
	type id = Vbd.id
	type position = Device_number.t option
	let positions = [ None; None; None ]
	let ids = List.map (fun x -> example_uuid, x) [ "0"; "1"; "2" ]
	let make id position =
		let open Vbd in {
			Vbd.id = id;
			position = position;
			mode = ReadWrite;
			backend = ("self", "/dev/zero");
			ty = Disk;
			unpluggable = true;
			extra_backend_keys = [ "backend", "keys" ];
			extra_private_keys = [ "private", "keys" ];
		}
	let create = Client.VBD.create rpc
	let destroy = Client.VBD.destroy rpc
	let plug = Client.VBD.plug rpc
	let unplug = Client.VBD.unplug rpc
	let list = Client.VBD.list rpc
	let find id vbds = List.find (fun x -> x.Vbd.id = id) vbds
	let assert_equal vbd vbd' =
		let open Vbd in
		assert_equal ~msg:"id" ~printer:(fun (a, b) -> Printf.sprintf "%s.%s" a b) vbd.id vbd'.id;
		assert_equal ~msg:"mode" ~printer:(function ReadWrite -> "RW" | ReadOnly -> "RO") vbd.mode vbd'.mode;
		assert_equal ~msg:"backend" ~printer:(fun (a, b) -> Printf.sprintf "%s.%s" a b) vbd.backend vbd'.backend;
		assert_equal ~msg:"unpluggable" ~printer:string_of_bool vbd.unpluggable vbd'.unpluggable;
		assert_equal ~msg:"extra_backend_keys" ~printer:sl vbd.extra_backend_keys vbd'.extra_backend_keys;
		assert_equal ~msg:"extra_private_keys" ~printer:sl vbd.extra_private_keys vbd'.extra_private_keys
end)

module VifDeviceTests = DeviceTests(struct
	type t = Vif.t
	type id = Vif.id
	type position = int
	let positions = [ 0; 1; 2 ]
	let ids = List.map (fun x -> example_uuid, x) [ "0"; "1"; "2" ]
	let make id position =
		let open Vif in {
			id = id;
			position = position;
			ty = Bridge "xenbr";
			mac = "c0:ff:ee:c0:ff:ee";
			carrier = false;
			mtu = 1450;
			rate = Some(1L, 2L);
			backend = "domN";
			other_config = [ "other", "config" ];
			extra_private_keys = [ "private", "keys" ];
		}
	let create = Client.VIF.create rpc
	let destroy = Client.VIF.destroy rpc
	let plug = Client.VIF.plug rpc
	let unplug = Client.VIF.unplug rpc
	let list = Client.VIF.list rpc
	let find id vifs = List.find (fun x -> x.Vif.id = id) vifs
	let assert_equal vif vif' =
		let open Vif in
		assert_equal ~msg:"id" ~printer:(fun (a, b) -> Printf.sprintf "%s.%s" a b) vif.id vif'.id;
		assert_equal ~msg:"position" ~printer:string_of_int vif.position vif'.position;
		assert_equal ~msg:"ty" ~printer:(function Bridge x -> "Bridge " ^ x | Vswitch x -> "Vswitch " ^ x) vif.ty vif'.ty;
		assert_equal ~msg:"mac" ~printer:(fun x -> x) vif.mac vif'.mac;
		assert_equal ~msg:"carrier" ~printer:string_of_bool vif.carrier vif'.carrier;
		assert_equal ~msg:"mtu" ~printer:string_of_int vif.mtu vif'.mtu;
		assert_equal ~msg:"rate" ~printer:(function Some (a, b) -> Printf.sprintf "Some %Ld %Ld" a b | None -> "None") vif.rate vif'.rate;
		assert_equal ~msg:"backend" ~printer:(fun x -> x) vif.backend vif'.backend;
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
			"vm_test_destroy_missing" >:: vm_test_destroy_missing;
			"vm_test_create_destroy" >:: vm_test_create_destroy;
			"vm_test_make_shutdown" >:: vm_test_make_shutdown;
			"vm_test_pause_unpause" >:: vm_test_pause_unpause;
			"vm_test_build_pause_unpause" >:: vm_test_build_pause_unpause;
			"vm_test_create_list_destroy" >:: vm_test_create_list_destroy;
			"vm_destroy_running" >:: vm_destroy_running;
			"vbd_test_create_destroy" >:: VbdDeviceTests.create_destroy;
			"vbd_test_create_list_destroy" >:: VbdDeviceTests.create_list_destroy;
			"vbd_test_create_vm_destroy" >:: VbdDeviceTests.create_vm_destroy;
			"vbd_test_create_plug_unplug_destroy" >:: VbdDeviceTests.create_plug_unplug_destroy;
			"vbd_test_create_plug_unplug_many_destroy" >:: VbdDeviceTests.create_plug_unplug_many_destroy;
			"vbd_destroy_running" >:: VbdDeviceTests.destroy_running;
			"vif_test_create_destroy" >:: VifDeviceTests.create_destroy;
			"vif_test_create_list_destroy" >:: VifDeviceTests.create_list_destroy;
			"vif_test_create_vm_destroy" >:: VifDeviceTests.create_vm_destroy;

(*
			"vm_test_suspend" >:: vm_test_suspend;
			"vm_test_resume" >:: vm_test_resume;
			"vif_test_create_plug_unplug_destroy" >:: VifDeviceTests.create_plug_unplug_destroy;
			"vif_test_create_plug_unplug_many_destroy" >:: VifDeviceTests.create_plug_unplug_many_destroy;
			"vif_destroy_running" >:: VifDeviceTests.destroy_running;
*)
		] in

	run_test_tt ~verbose:!verbose suite


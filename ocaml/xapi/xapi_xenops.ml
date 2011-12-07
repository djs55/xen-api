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

module D=Debug.Debugger(struct let name="xenops" end)
open D

open Listext
open Xenops_interface

let disk_of_vdi ~__context ~self =
	try
		let vdi = Db.VDI.get_record ~__context ~self in
		let content_id =
			if List.mem_assoc "content_id" vdi.API.vDI_other_config
			then List.assoc "content_id" vdi.API.vDI_other_config
			else vdi.API.vDI_location (* PR-1255 *) in
		Some (VDI content_id)
	with _ -> None

let backend_of_network ~__context ~self =
	let bridge = Db.Network.get_bridge ~__context ~self in
	VSwitch bridge (* PR-1255 *)

let builder_of_vm ~__context ~vm =
	let open Vm in
	let find f map default feature =
		try f (List.assoc feature map)
		with _ -> default in
	let string = find (fun x -> x) in
	let int = find int_of_string in
	let bool = find bool_of_string in

	match Helpers.boot_method_of_vm ~__context ~vm with
		| Helpers.HVM { Helpers.timeoffset = t } -> HVM {
			hap = true;
			shadow_multiplier = vm.API.vM_HVM_shadow_multiplier;
			timeoffset = string vm.API.vM_platform "0" "timeoffset";
			video_mib = int vm.API.vM_platform 4 "videoram";
			video = begin match string vm.API.vM_platform "cirrus" "vga" with
				| "std" -> Standard_VGA
				| "cirrus" -> Cirrus
				| x ->
					error "Unknown platform/vga option: %s (expected 'std' or 'cirrus')" x;
					Cirrus
			end;
			acpi = bool vm.API.vM_platform true "acpi";
			serial = Some (string vm.API.vM_platform "pty" "hvm_serial");
			keymap = Some (string vm.API.vM_platform "en-us" "keymap");
			vnc_ip = Some "0.0.0.0" (*None PR-1255*);
			pci_emulations = [];
			pci_passthrough = false;
			boot_order = string vm.API.vM_HVM_boot_params "cd" "order";
			qemu_disk_cmdline = false;
		}
		| Helpers.DirectPV { Helpers.kernel = k; kernel_args = ka; ramdisk = initrd } ->
			PV {
				boot = Direct { kernel = k; cmdline = ka; ramdisk = initrd };
				framebuffer = false
			}
		| Helpers.IndirectPV { Helpers.bootloader = b; extra_args = e; legacy_args = l; pv_bootloader_args = p; vdis = vdis } ->
			PV {
				boot = Indirect { bootloader = b; extra_args = e; legacy_args = l; bootloader_args = p; devices = List.filter_map (fun x -> disk_of_vdi ~__context ~self:x) vdis };
				framebuffer = false
			}

module MD = struct
	let of_vbd ~__context ~vm ~vbd =
		let hvm = vm.API.vM_HVM_boot_policy <> "" in
		let device_number = Device_number.of_string hvm vbd.API.vBD_userdevice in
		let open Vbd in {
			id = (vm.API.vM_uuid, Device_number.to_linux_device device_number);
			position = Some device_number;
			mode = if vbd.API.vBD_mode = `RO then ReadOnly else ReadWrite;
			backend = disk_of_vdi ~__context ~self:vbd.API.vBD_VDI;
			ty = if vbd.API.vBD_type = `Disk then Disk else CDROM;
			unpluggable = true;
			extra_backend_keys = [];
			extra_private_keys = [];
		}

	let of_vif ~__context ~vm ~vif =
		let open Vif in {
			id = (vm.API.vM_uuid, vif.API.vIF_device);
			position = int_of_string vif.API.vIF_device;
			mac = vif.API.vIF_MAC;
			carrier = true;
			mtu = Int64.to_int vif.API.vIF_MTU;
			rate = None;
			backend = backend_of_network ~__context ~self:vif.API.vIF_network;
			other_config = vif.API.vIF_other_config;
			extra_private_keys = []
		}

	let of_vm ~__context ~vm =
		let on_crash_behaviour = function
			| `preserve -> []
			| `coredump_and_restart -> [ Vm.Coredump; Vm.Start ]
			| `coredump_and_destroy -> [ Vm.Coredump; Vm.Shutdown ]
			| `restart
			| `rename_restart -> [ Vm.Start ]
			| `destroy -> [ Vm.Shutdown ] in
		let on_normal_exit_behaviour = function
			| `restart -> [ Vm.Start ]
			| `destroy -> [ Vm.Shutdown ] in
		let open Vm in {
			id = vm.API.vM_uuid;
			name = vm.API.vM_name_label;
			ssidref = 0l;
			xsdata = [];
			platformdata = vm.API.vM_platform;
			bios_strings = [];
			ty = builder_of_vm ~__context ~vm;
			suppress_spurious_page_faults = (try List.assoc "suppress-spurious-page-faults" vm.API.vM_other_config = "true" with _ -> false);
			machine_address_size = (try Some(int_of_string (List.assoc "machine-address-size" vm.API.vM_other_config)) with _ -> None);
			memory_static_max = vm.API.vM_memory_static_max;
			memory_dynamic_max = vm.API.vM_memory_dynamic_max;
			memory_dynamic_min = vm.API.vM_memory_dynamic_min;
			vcpus = Int64.to_int vm.API.vM_VCPUs_max;
			on_crash = on_crash_behaviour vm.API.vM_actions_after_crash;
			on_shutdown = on_normal_exit_behaviour vm.API.vM_actions_after_shutdown;
			on_reboot = on_normal_exit_behaviour vm.API.vM_actions_after_reboot;
		}		
end

(* Create an instance of Metadata.t, suitable for uploading to the xenops service *)
let create_metadata ~__context ~self =
	let vm = Db.VM.get_record ~__context ~self in
	let vbds = List.map (fun self -> Db.VBD.get_record ~__context ~self) vm.API.vM_VBDs in
	let vifs = List.map (fun self -> Db.VIF.get_record ~__context ~self) vm.API.vM_VIFs in
	let open Metadata in {
		vm = MD.of_vm ~__context ~vm;
		vbds = List.map (fun vbd -> MD.of_vbd ~__context ~vm ~vbd) vbds;
		vifs = List.map (fun vif -> MD.of_vif ~__context ~vm ~vif) vifs;
		domains = None
	}

open Xenops_interface
open Xenops_client
open Fun

let rec events_watch from =
	let events, next = Client.UPDATES.get from None |> success in
	let open Dynamic in
	let lines = List.map
		(function			| Vm_t(x, state) ->
				
				Printf.sprintf "VM %s" x.Vm.name
			| Vbd_t(x, state) ->
				Printf.sprintf "VBD %s.%s" (fst x.Vbd.id) (snd x.Vbd.id)
			| Vif_t(x, state) ->
				Printf.sprintf "VIF %s.%s" (fst x.Vif.id) (snd x.Vif.id)
			| Task_t x ->
				Printf.sprintf "Task %s %s" x.Task.id (x.Task.result |> Task.rpc_of_result |> Jsonrpc.to_string)
		) events in
	List.iter (fun x -> Printf.printf "%-8d %s\n" (Opt.unbox next) x) lines;
	flush stdout;
	events_watch next

let success_task id =
	let t = Client.TASK.stat id |> success in
	match t.Task.result with
	| Task.Completed _ -> t
	| Task.Failed (Failed_to_contact_remote_service x) ->
		failwith (Printf.sprintf "Failed to contact remote service on: %s\n" x)
	| Task.Failed x -> failwith (Jsonrpc.to_string (rpc_of_error x))
	| Task.Pending _ -> failwith "task pending"

let start ~__context ~self paused =
	let txt = create_metadata ~__context ~self |> Metadata.rpc_of_t |> Jsonrpc.to_string in
	let id = Client.VM.import_metadata txt |> success in
	Client.VM.start id |> success |> wait_for_task |> success_task |> ignore_task;
	if not paused
	then Client.VM.unpause id |> success |> wait_for_task |> success_task |> ignore_task	


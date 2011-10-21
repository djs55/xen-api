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

open Xenops_interface
open Xenops_utils
open Xenops_helpers
open Xenstore
open Pervasiveext
open Fun

let this_domid ~xs = int_of_string (xs.Xs.read "domid")

let uuid_of_vm vm = Uuid.uuid_of_string vm.Vm.id
let domid_of_uuid ~xc ~xs uuid =
	let all = Xenctrl.domain_getinfolist xc 0 in
	try
		let di = List.find (fun x -> Uuid.uuid_of_int_array x.Xenctrl.handle = uuid) all in
		Some di.Xenctrl.domid
	with Not_found -> None

let with_disk ~xc ~xs (backend_vm_id, params) f =
	let frontend_domid = this_domid ~xs in
	match domid_of_uuid ~xc ~xs (Uuid.uuid_of_string backend_vm_id) with
		| None ->
			debug "Failed to determine my own domain id!";
			raise (Exception Does_not_exist)
		| Some backend_domid ->
			let t = {
				Device.Vbd.mode = Device.Vbd.ReadOnly;
				device_number = None; (* we don't mind *)
				phystype = Device.Vbd.Phys;
				params = params;
				dev_type = Device.Vbd.Disk;
				unpluggable = true;
				protocol = None;
				extra_backend_keys = [];
				extra_private_keys = [];
				backend_domid = backend_domid;
			} in
			let device = Device.Vbd.add ~xs ~hvm:false t frontend_domid in
			let open Device_common in
			finally
				(fun () ->
					device.frontend.devid 
				|> Device_number.of_xenstore_key |> Device_number.to_linux_device 
				|> f)
				(fun () -> Device.clean_shutdown ~xs device)

module VM = struct
	open Vm

	let make_exn vm =
		let hvm = match vm.ty with HVM _ -> true | _ -> false in
		let create_info = {
			Domain.ssidref = vm.ssidref;
			hvm = hvm;
			hap = hvm;
			name = vm.name;
			xsdata = vm.xsdata;
			platformdata = vm.platformdata;
			bios_strings = vm.bios_strings;
		} in
		with_xc_and_xs
			(fun xc xs ->
				let domid = Domain.make ~xc ~xs create_info (uuid_of_vm vm) in
				if vm.suppress_spurious_page_faults
				then Domain.suppress_spurious_page_faults ~xc domid;
				Domain.set_machine_address_size ~xc domid vm.machine_address_size;
				return ()
			)
	let make = wrap make_exn

	let on_domain f vm =
		let uuid = uuid_of_vm vm in
		with_xc_and_xs
			(fun xc xs ->
				match domid_of_uuid ~xc ~xs uuid with
					| None -> throw Does_not_exist
					| Some domid ->
						wrap return (f xc xs domid)
			)

	let destroy = wrap (on_domain (fun xc xs -> Domain.destroy ~preserve_xs_vm:false ~xc ~xs))

	let pause = wrap (on_domain (fun xc _ -> Domain.pause ~xc))

	let unpause = wrap (on_domain (fun xc _ -> Domain.unpause ~xc))

	let build_domain_exn xc xs domid vm =
		let make_build_info kernel priv = {
			Domain.memory_max = vm.memory_max_kib;
			memory_target = vm.memory_target_kib;
			kernel = kernel;
			vcpus = vm.vcpus;
			priv = priv;
		} in
		(* We should prevent leaking files in our filesystem *)
		let kernel_to_cleanup = ref None in
		finally (fun () ->
			let build_info =
				match vm.ty with
					| HVM hvm_info ->
						let builder_spec_info = Domain.BuildHVM {
							Domain.shadow_multiplier = hvm_info.shadow_multiplier;
							timeoffset = hvm_info.timeoffset;
							video_mib = hvm_info.video_mib;
						} in
						make_build_info Domain.hvmloader builder_spec_info
					| PV { boot = Direct direct } ->
						let builder_spec_info = Domain.BuildPV {
							Domain.cmdline = direct.cmdline;
							ramdisk = direct.ramdisk;
						} in
						make_build_info direct.kernel builder_spec_info
					| PV { boot = Indirect { devices = [] } } ->
						raise (Exception No_bootable_device)
					| PV { boot = Indirect ( { devices = d :: _ } as i ) } ->
						with_disk ~xc ~xs d
							(fun dev ->
								let b = Bootloader.extract ~bootloader:i.bootloader 
									~legacy_args:i.legacy_args ~extra_args:i.extra_args
									~pv_bootloader_args:i.bootloader_args 
									~disk:dev ~vm:vm.Vm.id () in
								kernel_to_cleanup := Some b;
								let builder_spec_info = Domain.BuildPV {
									Domain.cmdline = b.Bootloader.kernel_args;
									ramdisk = b.Bootloader.initrd_path;
								} in
								make_build_info b.Bootloader.kernel_path builder_spec_info
							) in
			let arch = Domain.build ~xc ~xs build_info domid in
			debug "Built domid %d with architecture %s" domid (Domain.string_of_domarch arch);
		) (fun () -> Opt.iter Bootloader.delete !kernel_to_cleanup)

	let build_domain vm xc xs domid =
		try
			build_domain_exn xc xs domid vm
		with
			| Bootloader.Bad_sexpr x ->
				let m = Printf.sprintf "Bootloader.Bad_sexpr %s" x in
				debug "%s" m;
				raise (Exception (Internal_error m))
			| Bootloader.Bad_error x ->
				let m = Printf.sprintf "Bootloader.Bad_error %s" x in
				debug "%s" m;
				raise (Exception (Internal_error m))
			| Bootloader.Unknown_bootloader x ->
				let m = Printf.sprintf "Bootloader.Unknown_bootloader %s" x in
				debug "%s" m;
				raise (Exception (Internal_error m))
			| Bootloader.Error_from_bootloader (a, b) ->
				let m = Printf.sprintf "Bootloader.Error_from_bootloader (%s, [ %s ])" a (String.concat "; " b) in
				debug "%s" m;
				raise (Exception (Bootloader_error (a, b)))
			| e ->
				let m = Printf.sprintf "Bootloader error: %s" (Printexc.to_string e) in
				debug "%s" m;
				raise (Exception (Internal_error m))

	let build vm = on_domain (build_domain vm) vm

	let suspend vm disk = throw Unimplemented
	let resume vm disk = throw Unimplemented

	let get_power_state vm =
		let uuid = uuid_of_vm vm in
		with_xc_and_xs
			(fun xc xs ->
				match domid_of_uuid ~xc ~xs uuid with
					| None -> return Halted
					| Some _ ->
						return Running
			)
end

module VBD = struct
	let plug vm vbd = throw Unimplemented
	let unplug vm vbd = throw Unimplemented

	let get_currently_attached vm vbd = throw Unimplemented
end

module VIF = struct
	let plug vm vbd = throw Unimplemented
	let unplug vm vbd = throw Unimplemented

	let get_currently_attached vm vbd = throw Unimplemented
end

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
open Threadext
open Fun

let this_domid ~xs = int_of_string (xs.Xs.read "domid")

let uuid_of_vm vm = Uuid.uuid_of_string vm.Vm.id
let di_of_uuid ~xc ~xs uuid =
	let all = Xenctrl.domain_getinfolist xc 0 in
	try
		let di = List.find (fun x -> Uuid.uuid_of_int_array x.Xenctrl.handle = uuid) all in
		Some di
	with Not_found -> None
let domid_of_uuid ~xc ~xs uuid = Opt.map (fun di -> di.Xenctrl.domid) (di_of_uuid ~xc ~xs uuid)

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

module Mem = struct
	let call_daemon xs fn args = Squeezed_rpc.Rpc.client ~xs ~service:Squeezed_rpc._service ~fn ~args
	let ignore_results (_: (string * string) list) = ()

	let wrap f =
		try f ()
		with
			| Squeezed_rpc.Error(code, descr) -> raise (Exception (Ballooning_error(code, descr)))
			| Squeezed_rpc.Server_not_registered -> raise (Exception (No_ballooning_service))

	let do_login_exn ~xs =
		let args = [ Squeezed_rpc._service_name, "xenopsd" ] in
		let results = call_daemon xs Squeezed_rpc._login args in
		List.assoc Squeezed_rpc._session_id results
	let do_login ~xs = wrap (fun () -> do_login_exn ~xs)

	(** Maintain a cached login session with the ballooning service; return the cached value on demand *)
	let get_session_id =
		let session_id = ref None in
		let m = Mutex.create () in
		fun ~xs ->
			Mutex.execute m
				(fun () ->
					match !session_id with
						| Some x -> x
						| None ->
							let s = do_login ~xs in
							session_id := Some s;
							s
				)

	(** If we fail to allocate because VMs either failed to co-operate or because they are still booting
		and haven't written their feature-balloon flag then retry for a while before finally giving up.
		In particular this should help smooth over the period when VMs are booting and haven't loaded their balloon
		drivers yet. *)
	let retry f =
		let start = Unix.gettimeofday () in
		let interval = 10. in
		let timeout = 60. in
		let rec loop () =
			try
				f ()
			with
				| Squeezed_rpc.Error(code, descr) as e when
					false
					|| code = Squeezed_rpc._error_domains_refused_to_cooperate_code
					|| code = Squeezed_rpc._error_cannot_free_this_much_memory_code ->
				let now = Unix.gettimeofday () in
				if now -. start > timeout then raise e else begin
					debug "Sleeping %.0f before retrying" interval;
					Thread.delay interval;
					loop ()
				end in
		loop ()

	(** Reserve a particular amount of memory and return a reservation id *)
	let reserve_memory_range_exn ~xc ~xs ~min ~max =
		let session_id = get_session_id ~xs in
		let reserved_memory, reservation_id =
			retry
				(fun () ->
					debug "reserve_memory_range min=%Ld max=%Ld" min max;
					let args = [ Squeezed_rpc._session_id, session_id; Squeezed_rpc._min, Int64.to_string min; Squeezed_rpc._max, Int64.to_string max ] in
					let results = call_daemon xs Squeezed_rpc._reserve_memory_range args in
					let kib = List.assoc Squeezed_rpc._kib results
					and reservation_id = List.assoc Squeezed_rpc._reservation_id results in
					debug "reserve_memory_range actual = %s" kib;
					Int64.of_string kib, reservation_id
				)
		in
		debug "reserved_memory = %Ld; min = %Ld; max = %Ld" reserved_memory min max;
		(* Post condition: *)
		assert (reserved_memory >= min);
		assert (reserved_memory <= max);
		reserved_memory, reservation_id
	let reserve_memory_range ~xc ~xs ~min ~max =
		wrap (fun () -> reserve_memory_range_exn ~xc ~xs ~min ~max)

	(** Delete a reservation given by [reservation_id] *)
	let delete_reservation_exn ~xs ~reservation_id =
		let session_id = get_session_id ~xs in
		debug "delete_reservation %s" reservation_id;
		let args = [ Squeezed_rpc._session_id, session_id; Squeezed_rpc._reservation_id, reservation_id ] in
		ignore_results (call_daemon xs Squeezed_rpc._delete_reservation args)
	let delete_reservation ~xs ~reservation_id =
		wrap (fun () -> delete_reservation_exn ~xs ~reservation_id)

	(** Reserves memory, passes the id to [f] and cleans up afterwards. If the user
		wants to keep the memory, then call [transfer_reservation_to_domain]. *)
	let with_reservation ~xc ~xs ~min ~max f =
		let amount, id = reserve_memory_range ~xc ~xs ~min ~max in
		finally
			(fun () -> f amount id)
			(fun () -> delete_reservation ~xs ~reservation_id:id)

	(** Transfer this 'reservation' to the given domain id *)
	let transfer_reservation_to_domain_exn ~xs ~reservation_id ~domid =
		let session_id = get_session_id ~xs in
		debug "transfer_reservation_to_domain %s -> %d" reservation_id domid;
		let args = [ Squeezed_rpc._session_id, session_id; Squeezed_rpc._reservation_id, reservation_id; Squeezed_rpc._domid, string_of_int domid ] in
		ignore_results (call_daemon xs Squeezed_rpc._transfer_reservation_to_domain args)
	let transfer_reservation_to_domain ~xs ~reservation_id ~domid =
		wrap (fun () -> transfer_reservation_to_domain_exn ~xs ~reservation_id ~domid)

	(** After an event which frees memory (eg a domain destruction), perform a one-off memory rebalance *)
	let balance_memory ~xc ~xs =
		debug "rebalance_memory";
		ignore_results (call_daemon xs Squeezed_rpc._balance_memory [])

end

module VM = struct
	open Vm

	let is_hvm vm = match vm.ty with HVM _ -> true | _ -> false

	let compute_overhead vm =
		let static_max_mib = Memory.mib_of_bytes_used vm.memory_static_max in
		let multiplier = match vm.ty with HVM hvm_info -> hvm_info.shadow_multiplier | _ -> 1. in
		let memory_overhead_mib =
			(if is_hvm vm then Memory.HVM.overhead_mib else Memory.Linux.overhead_mib)
			static_max_mib vm.vcpus multiplier in
		Memory.bytes_of_mib memory_overhead_mib

	(* We compute our initial target at memory reservation time, done before the domain
	   is created. We consume this information later when the domain is built. *)
	let set_initial_target ~xs domid initial_target =
		xs.Xs.write (Printf.sprintf "/local/domain/%d/memory/initial-target" domid)
			(Int64.to_string initial_target)
	let get_initial_target ~xs domid =
		Int64.of_string (xs.Xs.read (Printf.sprintf "/local/domain/%d/memory/initial-target" domid))

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
				let open Memory in
				let overhead_bytes = compute_overhead vm in
				let min_kib = kib_of_bytes_used (vm.memory_dynamic_min +++ overhead_bytes) in
				let max_kib = kib_of_bytes_used (vm.memory_dynamic_max +++ overhead_bytes) in
				Mem.with_reservation ~xc ~xs ~min:min_kib ~max:max_kib
					(fun target_plus_overhead_kib reservation_id ->
						let domid = Domain.make ~xc ~xs create_info (uuid_of_vm vm) in
						Mem.transfer_reservation_to_domain ~xs ~reservation_id ~domid;
						let initial_target =
							let target_plus_overhead_bytes = bytes_of_kib target_plus_overhead_kib in
							let target_bytes = target_plus_overhead_bytes --- overhead_bytes in
							min vm.memory_dynamic_max target_bytes in
						set_initial_target ~xs domid initial_target;
						if vm.suppress_spurious_page_faults
						then Domain.suppress_spurious_page_faults ~xc domid;
						Domain.set_machine_address_size ~xc domid vm.machine_address_size;
						return ()
					)
			)
	let make = wrap make_exn

	let on_domain f vm =
		let uuid = uuid_of_vm vm in
		with_xc_and_xs
			(fun xc xs ->
				match di_of_uuid ~xc ~xs uuid with
					| None -> throw Does_not_exist
					| Some di ->
						wrap return (f xc xs di)
			)

	let destroy = wrap (on_domain (fun xc xs di -> Domain.destroy ~preserve_xs_vm:false ~xc ~xs di.Xenctrl.domid))

	let pause = wrap (on_domain (fun xc xs di ->
		if di.Xenctrl.total_memory_pages = 0n then raise (Exception Domain_not_built);
		Domain.pause ~xc di.Xenctrl.domid
	))

	let unpause = wrap (on_domain (fun xc xs di ->
		if di.Xenctrl.total_memory_pages = 0n then raise (Exception Domain_not_built);
		Domain.unpause ~xc di.Xenctrl.domid
	))

	let build_domain_exn xc xs domid vm =
		let open Memory in
		let initial_target = get_initial_target ~xs domid in
		let make_build_info kernel priv = {
			Domain.memory_max = vm.memory_static_max /// 1024L;
			memory_target = initial_target /// 1024L;
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
			Domain.cpuid_apply ~xc ~hvm:(is_hvm vm) domid;
		) (fun () -> Opt.iter Bootloader.delete !kernel_to_cleanup)

	let build_domain vm xc xs di =
		try
			build_domain_exn xc xs di.Xenctrl.domid vm
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

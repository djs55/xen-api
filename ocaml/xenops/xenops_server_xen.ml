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

module VM = struct
	open Vm

	let uuid_of_vm vm = Uuid.uuid_of_string vm.id
	let domid_of_uuid ~xc ~xs uuid =
		let all = Xenctrl.domain_getinfolist xc 0 in
		try
			let di = List.find (fun x -> Uuid.uuid_of_int_array x.Xenctrl.handle = uuid) all in
			Some di.Xenctrl.domid
		with Not_found -> None

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

	let destroy_exn vm =
		let uuid = uuid_of_vm vm in
		with_xc_and_xs
			(fun xc xs ->
				match domid_of_uuid ~xc ~xs uuid with
					| None -> throw Does_not_exist
					| Some domid ->
						Domain.destroy xc xs domid;
						(* XXX: clean up xenstore stuff *)
						(* wait? *)
						return ()
			)
	let destroy = wrap destroy_exn

	let build vm = throw Unimplemented
	let pause vm = throw Unimplemented
	let unpause vm = throw Unimplemented

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

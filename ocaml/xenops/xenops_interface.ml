(*
 * Copyright (C) 2011 Citrix Systems Inc.
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
 * @group Xenops
 *)

type ('a, 'b) result =
	| Success of 'a
	| Failure of 'b

type generic_error =
	| Internal_error of string

module Query = struct
	type t = {
		name: string;
		vendor: string;
		version: string;
		features: string list;
	}
end
external query: unit -> (Query.t option * generic_error option) = ""

module Vm = struct
	type hvm_info = {
		hap: bool;
		shadow_multiplier: float;
		timeoffset: string;
		video_mib: int;
	}

	type pv_info = {
		cmdline: string;
		ramdisk: string option;
	}

	type builder_info =
	| HVM of hvm_info
	| PV of pv_info

	type id = string

	type t = {
		vm: id;
		domid: int option;
		name: string;
		ssidref: int32;
		xsdata: (string * string) list;
		platformdata: (string * string) list;
		bios_strings: (string * string) list;
		ty: builder_info;
	}

	type create_error =
		| Already_exists of id

	type destroy_error =
		| Does_not_exist of id
end

module VM = struct
	external create: Vm.t -> (Vm.id option) * (Vm.create_error option) = ""
	external destroy: Vm.id -> (unit option) * (Vm.destroy_error option) = ""
	external list: unit -> (Vm.t list option) * (generic_error option) = ""
end


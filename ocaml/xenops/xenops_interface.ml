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

type error =
	| Internal_error of string
	| Already_exists
	| Does_not_exist

module Query = struct
	type t = {
		name: string;
		vendor: string;
		version: string;
		features: string list;
	}
end
external query: unit -> (Query.t option * error option) = ""

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
		id: id;
		domid: int option;
		name: string;
		ssidref: int32;
		xsdata: (string * string) list;
		platformdata: (string * string) list;
		bios_strings: (string * string) list;
		ty: builder_info;
	}

end

module VM = struct
	external create: Vm.t -> (Vm.id option) * (error option) = ""
	external destroy: Vm.id -> (unit option) * (error option) = ""
	external list: unit -> (Vm.t list option) * (error option) = ""
end

module Vbd = struct

	type mode = ReadOnly | ReadWrite

	type ty = CDROM | Disk

	type id = string * string

	type t = {
		id: id;
		mode: mode;
		backend: string * string; (* vm.id * params *)
		ty: ty;
		unpluggable: bool;
		extra_backend_keys: (string * string) list;
		extra_private_keys: (string * string) list;
	}

end

module VBD = struct
	external create: Vbd.t -> (Vbd.id option) * (error option) = ""
	external list: Vm.id -> (Vbd.t list option) * (error option) = ""
	external destroy: Vbd.id -> (unit option) * (error option) = ""
end

module Vif = struct

	type ty =
		| Bridge of string
		| Vswitch of string

	type id = string * string

	type t = {
		id: id;
		ty: ty;
		mac: string;
		carrier: bool;
		mtu: int;
		rate: (int64 * int64) option;
		backend: string;
		other_config: (string * string) list;
		extra_private_keys: (string * string) list;
	}
end

module VIF = struct
	external create: Vif.t -> (Vif.id option) * (error option) = ""
	external list: Vm.id -> (Vif.t list option) * (error option) = ""
	external destroy: Vif.id -> (unit option) * (error option) = ""
end

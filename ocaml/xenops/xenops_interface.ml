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

type power_state =
	| Halted
	| Running

type error =
	| Internal_error of string
	| Already_exists
	| Does_not_exist
	| Unimplemented
	| Domain_not_built
	| Bad_power_state of power_state * power_state
	| Device_is_connected
	| Device_not_connected
	| No_bootable_device
	| Bootloader_error of string * (string list)
	| Ballooning_error of string * string
	| No_ballooning_service

module Query = struct
	type t = {
		name: string;
		vendor: string;
		version: string;
		features: string list;
	}
end
external query: unit -> (Query.t option * error option) = ""

type disk = string * string (* vm.id * params *)

module Vm = struct
	type hvm_info = {
		hap: bool;
		shadow_multiplier: float;
		timeoffset: string;
		video_mib: int;
	}

	type pv_direct_boot = {
		kernel: string;
		cmdline: string;
		ramdisk: string option;
	}

	type pv_indirect_boot = {
		bootloader: string;
		extra_args: string;
		legacy_args: string;
		bootloader_args: string;
		devices: disk list;
	}

	type pv_boot =
		| Direct of pv_direct_boot
		| Indirect of pv_indirect_boot

	type pv_info = {
		boot: pv_boot;
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
		suppress_spurious_page_faults: bool;
		machine_address_size: int option;
		memory_static_max: int64;
		memory_dynamic_max: int64;
		memory_dynamic_min: int64;
		vcpus: int;
	}
end

module Vbd = struct

	type mode = ReadOnly | ReadWrite

	type ty = CDROM | Disk

	type id = string * string

	(* FIXME: take a URL and call VDI.attach ourselves *)

	type t = {
		id: id;
		position: Device_number.t option;
		mode: mode;
		backend: disk;
		ty: ty;
		unpluggable: bool;
		extra_backend_keys: (string * string) list;
		extra_private_keys: (string * string) list;
	}

end

module Vif = struct

	type ty =
		| Bridge of string
		| Vswitch of string

	type id = string * string

	type t = {
		id: id;
		position: int;
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

module VM = struct
	external create: Vm.t -> (Vm.id option) * (error option) = ""
	external destroy: Vm.id -> (unit option) * (error option) = ""
	external make: Vm.id -> (unit option) * (error option) = ""
	external build: Vm.id -> (unit option) * (error option) = ""
	external shutdown: Vm.id -> (unit option) * (error option) = ""
	external pause: Vm.id -> (unit option) * (error option) = ""
	external unpause: Vm.id -> (unit option) * (error option) = ""
	external list: unit -> (Vm.t list option) * (error option) = ""

	external suspend: Vm.id -> disk -> (unit option) * (error option) = ""
	external resume: Vm.id -> disk -> (unit option) * (error option) = ""
end

module VBD = struct
	external create: Vbd.t -> (Vbd.id option) * (error option) = ""
	external plug: Vbd.id -> (unit option) * (error option) = ""
	external unplug: Vbd.id -> (unit option) * (error option) = ""
	external list: Vm.id -> (Vbd.t list option) * (error option) = ""
	external destroy: Vbd.id -> (unit option) * (error option) = ""
end

module VIF = struct
	external create: Vif.t -> (Vif.id option) * (error option) = ""
	external plug: Vif.id -> (unit option) * (error option) = ""
	external unplug: Vif.id -> (unit option) * (error option) = ""
	external list: Vm.id -> (Vif.t list option) * (error option) = ""
	external destroy: Vif.id -> (unit option) * (error option) = ""
end

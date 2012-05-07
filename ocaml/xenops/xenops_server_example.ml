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
open Xenops_server_plugin
open Xenops_utils

module D = Debug.Debugger(struct let name = "example" end)
open D

let mib = Int64.mul 1024L 1024L
let gib = Int64.mul mib 1024L

(* Used to signal when work needs to be done on a VM *)
let updates = Updates.empty ()

module Backend = struct

module HOST = struct
	let get_console_data () = "the console contents"

	let get_total_memory_mib () = Int64.(mul 8L gib)

	let send_debug_keys keys = ()
end

module VM = struct
	let generate_state_string vm = ""

	let create task memory_limit vm = ()

	let destroy_device_model task vm = ()

	let destroy task vm = ()

	let pause task vm = ()

	let unpause task vm = ()

	let set_xsdata task vm xsdata = ()

	let set_vcpus task vm target = ()

	let set_shadow_multiplier task vm target = ()

	let set_memory_dynamic_range task vm min max = ()

	let build task vm vbds vifs = ()

	let create_device_model task vm save_state = ()

	let request_shutdown task vm reason ack_delay = true

	let wait_shutdown task vm reason timeout = true

	let save task progress_callback vm flags data = ()

	let restore task progress_callback vm vbds vifs data = ()

	let s3suspend task vm = ()

	let s3resume task vm = ()

	let get_state vm = failwith "get_state"

	let set_domain_action_request vm request = ()

	let get_domain_action_request vm = None

	let get_internal_state vdi_map vif_map vm = failwith "get_internal_state"

	let set_internal_state vm state = failwith "set_internal_state"

	let minimum_reboot_delay = 15.
end

module PCI = struct
	let get_state vm pci = failwith "get_state"

	let get_device_action_request vm pci = None

	let plug task vm pci = ()

	let unplug task vm pci = ()
end

module VBD = struct
	let plug task vm vbd = ()

	let unplug task vm vbd force = ()

	let insert task vm vbd disk = ()

	let eject task vm vbd = ()

	let set_qos task vm vbd = ()

	let get_qos xc xs vm vbd device = None

	let get_state vm vbd = failwith "get_state"

	let get_device_action_request vm vbd = None
end

module VIF = struct
	let plug task vm vif = ()

	let unplug task vm vif force = ()

	let move task vm vif network = ()

	let set_carrier task vm vif carrier = ()

	let set_locking_mode task vm vif mode = ()

	let get_state vm vif = failwith "get_state"

	let get_device_action_request vm vif = None
end

module UPDATES = struct
	let get last timeout = Updates.get "UPDATES.get" last timeout updates
end

let init () = ()

module DEBUG = struct
	let trigger cmd args = ()
end

end

let _ = Xenops_server.register_backend "example" (module Backend: Xenops_server_plugin.S)

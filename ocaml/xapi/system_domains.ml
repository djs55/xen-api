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
 * @group Helper functions for handling system domains
 *)

let system_domain_key = "is_system_domain"

let bool_of_string x = try bool_of_string x with _ -> false

let is_system_domain snapshot =
	let oc = snapshot.API.vM_other_config in
	List.mem_assoc system_domain_key oc && (bool_of_string (List.assoc system_domain_key oc))

let get_is_system_domain ~__context ~self =
	is_system_domain (Db.VM.get_record ~__context ~self)

let set_is_system_domain ~__context ~self ~value =
	Db.VM.remove_from_other_config ~__context ~self ~key:system_domain_key;
	Db.VM.add_to_other_config ~__context ~self ~key:system_domain_key ~value

(* [wait_for_ip ?timeout ip] returns true if [ip] becomes pingable within [timeout], 
   false otherwise *)
let wait_for_ip ?(timeout=120.) ip =
	let start = Unix.gettimeofday () in
	let finished = ref false in
	let success = ref false in
	while not(!finished) do
		let remaining = timeout -. (Unix.gettimeofday () -. start) in
		if remaining < 0. 
		then finished := true
		else 
			try
				let (_: string * string) = Forkhelpers.execute_command_get_output "ping" [ "-c"; "1"; "-w"; string_of_int (int_of_float (remaining +. 1.)) ] in
				success := true;
				finished := true
			with _ ->
				Thread.delay 1.
	done;
	!success

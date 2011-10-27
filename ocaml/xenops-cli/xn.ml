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

open Fun

let usage () =
	Printf.fprintf stderr "%s <command> [args] - send commands to the xenops daemon\n" Sys.argv.(0);
	Printf.fprintf stderr "%s create <config> - create a VM from <config>\n" Sys.argv.(0)

open Xenops_interface
open Xmlrpc_client
let default_path = "/var/xapi/xenopsd"
let transport = ref (Unix default_path)

let rpc call =
	XMLRPC_protocol.rpc ~transport:!transport
		~http:(xmlrpc ~version:"1.0" "/") call

let success = function
	| (_, Some x) -> failwith (Jsonrpc.to_string (rpc_of_error x))
	| (Some x, _) -> x
	| None, None -> failwith "protocol error"

let create filename =
	Unixext.with_input_channel filename
		(fun ic ->
			let lexbuf = Lexing.from_channel ic in
			let config = Xn_cfg_parser.file Xn_cfg_lexer.token lexbuf in
			let open Xn_cfg_types in
			let mem x = List.mem_assoc x config in
			let find x = List.assoc x config in
			let any xs = List.fold_left (||) false (List.map mem xs) in
			let pv =
				false
				|| (mem _builder && (find _builder |> string = "linux"))
				|| (not(mem _builder) && (any [ _bootloader; _kernel ])) in
			let open Vm in
			let builder_info = match pv with
				| true -> PV {
					boot =
						if mem _bootloader then Indirect {
							bootloader = find _bootloader |> string;
							extra_args = "";
							legacy_args = "";
							bootloader_args = "";
							devices = [];
						} else if mem _kernel then Direct {
							kernel = find _kernel |> string;
							cmdline = if mem _root then find _root |> string else "";
							ramdisk = if mem _ramdisk then Some (find _ramdisk |> string) else None;
						} else begin
							List.iter (Printf.fprintf stderr "%s\n") [
								"I couldn't determine how to start this VM.";
								Printf.sprintf "A PV guest needs either %s or %s and %s" _bootloader _kernel _ramdisk
							];
							exit 1
						end
					}
				| false -> HVM {
					hap = true;
					shadow_multiplier = 1.;
					timeoffset = "";
					video_mib = 4;
				} in
			let uuid = if mem _uuid then find _uuid |> string else "c0ffeec0-ffee-c0ff-eec0-ffeec0ffeec0" in
			let name = if mem _name then find _name |> string else uuid in
			let mib = if mem _memory then find _memory |> int |> Int64.of_int else 64L in
			let bytes = Int64.mul 1024L (Int64.mul 1024L mib) in
			let vcpus = if mem _vcpus then find _vcpus |> int else 1 in
			let vm = {
				id = uuid;
				domid = None;
				name = name;
				ssidref = 0l;
				xsdata = [];
				platformdata = [];
				bios_strings = [];
				ty = builder_info;
				suppress_spurious_page_faults = false;
				machine_address_size = None;
				memory_static_max = bytes;
				memory_dynamic_max = bytes;
				memory_dynamic_min = bytes;
				vcpus = vcpus
			} in
			let (id: Vm.id) = success (Client.VM.create rpc vm) in
			Printf.printf "%s\n" id
		)

let list () =
	let open Vm in
	let vms = success (Client.VM.list rpc ()) in
	let string_of_vm vm =
		Printf.sprintf "%s %s %s" vm.id (Opt.default "None" (Opt.map (Printf.sprintf "%3d") vm.domid)) vm.name in
	List.iter (Printf.printf "%s\n") (List.map string_of_vm vms)

let find_by_name x =
	let open Vm in
	let all = success (Client.VM.list rpc ()) in
	let this_one y = y.id = x || y.name = x in
	try
		List.find this_one all
	with Not_found ->
		Printf.fprintf stderr "Failed to find VM: %s\n" x;
		exit 1

let destroy x =
	let open Vm in
	let vm = find_by_name x in
	success (Client.VM.destroy rpc vm.id)

let start x =
	let open Vm in
	let vm = find_by_name x in
	success (Client.VM.make rpc vm.id);
	success (Client.VM.build rpc vm.id);
	success (Client.VM.unpause rpc vm.id)

let shutdown x =
	let open Vm in
	let vm = find_by_name x in
	success (Client.VM.shutdown rpc vm.id)

let _ =
	match List.tl (Array.to_list Sys.argv) with
		| [ "help" ] | [] ->
			usage ();
			exit 0
		| [ "create"; filename ] ->
			create filename
		| [ "list" ] ->
			list ()
		| [ "destroy"; id ] ->
			destroy id
		| [ "start"; id ] ->
			start id
		| [ "shutdown"; id ] ->
			shutdown id
		| cmd :: _ ->
			Printf.fprintf stderr "Unrecognised command: %s\n" cmd;
			usage ();
			exit 1

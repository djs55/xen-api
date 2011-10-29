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

open Stringext
open Fun

let usage () =
	Printf.fprintf stderr "%s <command> [args] - send commands to the xenops daemon\n" Sys.argv.(0);
	Printf.fprintf stderr "%s add <config> - add a VM from <config>\n" Sys.argv.(0)

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

let add filename =
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
					framebuffer = false;
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
					video = Cirrus;
					acpi = true;
					serial = None;
					keymap = None;
					vnc_ip = Some "0.0.0.0";
					pci_emulations = [];
					pci_passthrough = false;
					boot_order = if mem _boot then find _boot |> string else "cd";
					qemu_disk_cmdline = false;
				} in
			let uuid = if mem _uuid then find _uuid |> string else "c0ffeec0-ffee-c0ff-eec0-ffeec0ffeec0" in
			let name = if mem _name then find _name |> string else uuid in
			let mib = if mem _memory then find _memory |> int |> Int64.of_int else 64L in
			let bytes = Int64.mul 1024L (Int64.mul 1024L mib) in
			let vcpus = if mem _vcpus then find _vcpus |> int else 1 in
			let vm = {
				id = uuid;
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
			let (id: Vm.id) = success (Client.VM.add rpc vm) in
			let disks = if mem _disk then find _disk |> list string else [] in
			let parse_disk x = match String.split ',' x with
				| [ source; device_number; rw ] ->
					let ty, device_number' = match String.split ':' device_number with
						| [ x ] -> Vbd.Disk, Device_number.of_string false x
						| [ x; "cdrom" ] -> Vbd.CDROM, Device_number.of_string false x
						| _ ->
							Printf.fprintf stderr "Failed to understand disk name '%s'. It should be 'xvda' or 'hda:cdrom'\n" device_number;
							exit 2 in
					let mode = match String.lowercase rw with
						| "r" -> Vbd.ReadOnly
						| "w" -> Vbd.ReadWrite
						| x ->
							Printf.fprintf stderr "Failed to understand disk mode '%s'. It should be 'r' or 'w'\n" x;
							exit 2 in
					let backend = match List.filter (fun x -> x <> "") (String.split ':' source) with
						| [ "phy"; path ] -> Some (Local path)
						| [ "file"; path ] ->
							Printf.fprintf stderr "I don't understand 'file' disk paths. Please use 'phy'.\n";
							exit 2
						| [] -> None (* empty *)
						| _ ->
							Printf.fprintf stderr "I don't understand '%s'. Please use 'phy:path,...\n" source;
							exit 2 in {
						Vbd.id = id, device_number;
						position = Some device_number';
						mode = mode;
						backend = backend;
						ty = ty;
						unpluggable = true;
						extra_backend_keys = [];
						extra_private_keys = [];
					}
				| _ ->
					Printf.fprintf stderr "I don't understand '%s'. Please use 'phy:path,xvda,w'\n" x;
					exit 2 in
			let one x = x |> parse_disk |> Client.VBD.add rpc |> success in
			let (_: Vbd.id list) = List.map one disks in
			let vifs = if mem _disk then find _vif |> list string else [] in
			let vifs = List.combine vifs (Range.to_list (Range.make 0 (List.length vifs))) in
			let parse_vif (x, idx) =
				let xs = List.filter (fun x -> x <> "") (List.map (String.strip String.isspace) (String.split ',' x)) in
				let kvpairs = List.map (fun x -> match String.split ~limit:2 '=' x with
					| [ k; v ] -> k, v
					| _ ->
						Printf.fprintf stderr "I don't understand '%s'. Please use 'mac=xx:xx:xx:xx:xx:xx,bridge=xenbrX'.\n" x;
						exit 2
				) xs in {
					Vif.id = id, string_of_int idx;
					position = idx;
					mac = if List.mem_assoc _mac kvpairs then List.assoc _mac kvpairs else "c0:ff:ee:c0:ff:ee";
					carrier = true;
					mtu = 1500;
					rate = None;
					backend = if List.mem_assoc _bridge kvpairs then VSwitch (List.assoc _bridge kvpairs) else VSwitch "xenbr0";
					other_config = [];
					extra_private_keys = [];
				} in
			let one x = x |> parse_vif |> Client.VIF.add rpc |> success in
			let (_: Vif.id list) = List.map one vifs in
			Printf.printf "%s\n" id
		)

let list () =
	let open Vm in
	let line name domid mem vcpus state time =
		Printf.sprintf "%-45s%-5s%-4s%-5s     %-8s%-s" name domid mem vcpus state time in
	let header = line "Name" "ID" "Mem" "VCPUs" "State" "Time(s)" in
	let string_of_vm (vm, power_state) =
		let domid = match power_state with
			| Running { domid = d } -> string_of_int d
			| _ -> "-" in
		let mem = Int64.to_string (Int64.div (Int64.div vm.memory_static_max 1024L) 1024L) in
		let vcpus = string_of_int vm.vcpus in
		let state = match power_state with
			| Running _ -> "Running"
			| Suspended -> "Suspend"
			| Halted    -> "Halted "
			| Paused    -> "Paused " in
		line vm.name domid mem vcpus state "" in
	let vms = success (Client.VM.list rpc ()) in
	let lines = header :: (List.map string_of_vm vms) in
	List.iter (Printf.printf "%s\n") lines

let find_by_name x =
	let open Vm in
	let all = success (Client.VM.list rpc ()) in
	let this_one (y, _) = y.id = x || y.name = x in
	try
		List.find this_one all
	with Not_found ->
		Printf.fprintf stderr "Failed to find VM: %s\n" x;
		exit 1

let remove x =
	let open Vm in
	let vm, _ = find_by_name x in
	let vbds = success (Client.VBD.list rpc vm.id) in
	List.iter
		(fun vbd ->
			success (Client.VBD.remove rpc vbd.Vbd.id)
		) vbds;
	let vifs = success (Client.VIF.list rpc vm.id) in
	List.iter
		(fun vif ->
			success (Client.VIF.remove rpc vif.Vif.id)
		) vifs;
	success (Client.VM.remove rpc vm.id)

let start x =
	let open Vm in
	let vm, _ = find_by_name x in
	success (Client.VM.start rpc vm.id)

let shutdown x =
	let open Vm in
	let vm, _ = find_by_name x in
	success (Client.VM.shutdown rpc vm.id)

let pause x =
	let open Vm in
	let vm, _ = find_by_name x in
	success (Client.VM.pause rpc vm.id)

let unpause x =
	let open Vm in
	let vm, _ = find_by_name x in
	success (Client.VM.unpause rpc vm.id)

let _ =
	match List.tl (Array.to_list Sys.argv) with
		| [ "help" ] | [] ->
			usage ();
			exit 0
		| [ "add"; filename ] ->
			add filename
		| [ "list" ] ->
			list ()
		| [ "remove"; id ] ->
			remove id
		| [ "start"; id ] ->
			start id
		| [ "pause"; id ] ->
			pause id
		| [ "unpause"; id ] ->
			unpause id
		| [ "shutdown"; id ] ->
			shutdown id
		| cmd :: _ ->
			Printf.fprintf stderr "Unrecognised command: %s\n" cmd;
			usage ();
			exit 1

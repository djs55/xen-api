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
open Threadext
open Pervasiveext
open Fun

let usage () =
	Printf.fprintf stderr "%s <command> [args] - send commands to the xenops daemon\n" Sys.argv.(0);
	Printf.fprintf stderr "%s add <config> - add a VM from <config>\n" Sys.argv.(0);
	Printf.fprintf stderr "%s list - query the states of known VMs\n" Sys.argv.(0);
	Printf.fprintf stderr "%s remove <name or id> - forget about a VM\n" Sys.argv.(0);
	Printf.fprintf stderr "%s start <name or id> - start a VM\n" Sys.argv.(0);
	Printf.fprintf stderr "%s pause <name or id> - pause a VM\n" Sys.argv.(0);
	Printf.fprintf stderr "%s unpause <name or id> - unpause a VM\n" Sys.argv.(0);
	Printf.fprintf stderr "%s shutdown <name or id> - shutdown a VM\n" Sys.argv.(0);
	Printf.fprintf stderr "%s reboot <name or id> - reboot a VM\n" Sys.argv.(0);
	Printf.fprintf stderr "%s suspend <name or id> <disk> - suspend a VM\n" Sys.argv.(0);
	Printf.fprintf stderr "%s resume <name or id> <disk> - resume a VM\n" Sys.argv.(0);
	Printf.fprintf stderr "%s migrate <name or id> - migrate a VM\n" Sys.argv.(0);
	Printf.fprintf stderr "%s vbd-list <name or id> - query the states of a VM's block devices\n" Sys.argv.(0);
	Printf.fprintf stderr "%s cd-insert <id> <disk> - insert a CD into a VBD\n" Sys.argv.(0);
	Printf.fprintf stderr "%s cd-eject <id> - eject a CD from a VBD\n" Sys.argv.(0);
	()


open Xenops_interface
open Xmlrpc_client
let default_path = "/var/xapi/xenopsd"
let forwarded_path = default_path ^ ".forwarded"
let transport = ref (Unix default_path)

let rpc call =
	XMLRPC_protocol.rpc ~transport:!transport
		~http:(xmlrpc ~version:"1.0" "/") call

let success = function
	| (_, Some x) -> failwith (Jsonrpc.to_string (rpc_of_error x))
	| (Some x, _) -> x
	| None, None -> failwith "protocol error"

let event_wait rpc p =
	let finished = ref false in
	let event_id = ref None in
	while not !finished do
		let deltas, next_id = Client.UPDATES.get rpc !event_id (Some 30) |> success in
		event_id := next_id;
		List.iter (fun d -> if p d then finished := true) deltas;
	done

let wait_for_task rpc id =
	Printf.fprintf stderr "wait_for id = %s\n%!" id;
	let finished = function
		| Dynamic.Task_t t ->
			Printf.fprintf stderr "got event for id %s\n%!" id;
			if t.Task.id = id then begin
				match t.Task.result with
				| Task.Pending _ -> false
				| Task.Completed -> true
				| Task.Failed _ -> true
			end else false
		| x ->
			Printf.fprintf stderr "ignore event on %s\n%!" (x |> Dynamic.rpc_of_t |> Jsonrpc.to_string);
			false in 
	event_wait rpc finished;
	id

let success_task rpc id =
	let t = Client.TASK.stat rpc id |> success in
	match t.Task.result with
	| Task.Completed -> ()
	| Task.Failed x -> failwith (Jsonrpc.to_string (rpc_of_error x))
	| Task.Pending _ -> failwith "task pending"

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
				platformdata = [ (* HVM defaults *)
					"nx", "false";
					"acpi", "true";
					"apic", "true";
					"pae", "true";
					"viridian", "true";
					"vcpu/number", "1";
					"vcpu/current", "1";
				];
				bios_strings = [];
				ty = builder_info;
				suppress_spurious_page_faults = false;
				machine_address_size = None;
				memory_static_max = bytes;
				memory_dynamic_max = bytes;
				memory_dynamic_min = bytes;
				vcpus = vcpus;
				on_crash = [ Vm.Shutdown ];
				on_shutdown = [ Vm.Shutdown ];
				on_reboot = [ Vm.Start ];
			} in
			let (id: Vm.id) = success (Client.VM.add rpc vm) in
			let disks = if mem _disk then find _disk |> list string else [] in
			let parse_disk x = match String.split ',' x with
				| [ source; device_number; rw ] ->
					let ty, device_number, device_number' = match String.split ':' device_number with
						| [ x ] -> Vbd.Disk, x, Device_number.of_string false x
						| [ x; "cdrom" ] -> Vbd.CDROM, x, Device_number.of_string false x
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
						| [ "sm"; path ] -> Some (VDI path)
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
			let vifs = if mem _vif then find _vif |> list string else [] in
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
					mac = if List.mem_assoc _mac kvpairs then List.assoc _mac kvpairs else "";
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
	let string_of_vm (vm, state) =
		let domid = match state.Vm.power_state with
			| Running -> String.concat "," (List.map string_of_int state.Vm.domids)
			| _ -> "-" in
		let mem = Int64.to_string (Int64.div (Int64.div vm.memory_static_max 1024L) 1024L) in
		let vcpus = string_of_int vm.vcpus in
		let state = match state.Vm.power_state with
			| Running   -> "Running"
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
		(fun (vbd, _) ->
			success (Client.VBD.remove rpc vbd.Vbd.id)
		) vbds;
	let vifs = success (Client.VIF.list rpc vm.id) in
	List.iter
		(fun (vif, _) ->
			success (Client.VIF.remove rpc vif.Vif.id)
		) vifs;
	success (Client.VM.remove rpc vm.id)

let start x =
	let open Vm in
	let vm, _ = find_by_name x in
	Client.VM.start rpc vm.id |> success |> wait_for_task rpc |> success_task rpc

let shutdown x =
	let open Vm in
	let vm, _ = find_by_name x in
	Client.VM.shutdown rpc vm.id |> success |> wait_for_task rpc |> success_task rpc

let pause x =
	let open Vm in
	let vm, _ = find_by_name x in
	Client.VM.pause rpc vm.id |> success |> wait_for_task rpc |> success_task rpc

let unpause x =
	let open Vm in
	let vm, _ = find_by_name x in
	Client.VM.unpause rpc vm.id |> success |> wait_for_task rpc |> success_task rpc

let reboot x timeout =
	let open Vm in
	let vm, _ = find_by_name x in
	Client.VM.reboot rpc vm.id timeout |> success |> wait_for_task rpc |> success_task rpc

let suspend x disk =
	let open Vm in
	let vm, _ = find_by_name x in
	Client.VM.suspend rpc vm.id (Local disk) |> success |> wait_for_task rpc |> success_task rpc

let resume x disk =
	let open Vm in
	let vm, _ = find_by_name x in
	Client.VM.resume rpc vm.id (Local disk) |> success |> wait_for_task rpc |> success_task rpc

let migrate x remotecmd =
	let open Vm in
	let vm, _ = find_by_name x in
	let a, b = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
	let toclose = ref [ a; b ] in
	let close x = if List.mem x !toclose then (Unix.close x; toclose := List.filter (fun y -> x <> y) !toclose) in
	finally
		(fun () ->
			let pid = Unix.create_process (List.hd remotecmd) (Array.of_list remotecmd) b b Unix.stderr in
			close b;
			(* Send 'a' as part of the VM.migrate operation *)
			let local = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
			Unix.connect local (Unix.ADDR_UNIX forwarded_path);
			let jsonrpc_request = Rpc.call "VM.migrate" [ Vm.rpc_of_id vm.id ] |> Jsonrpc.string_of_call in
			let buf = Xmlrpc_client.xmlrpc ~length:(String.length jsonrpc_request |> Int64.of_int) "/" |> Http.Request.rpc_of_t |> Jsonrpc.to_string in
			let n = Unixext.send_fd local buf 0 (String.length buf) [] a in
			close a;
			Printf.fprintf stderr "sent message = [%s]\n%!" buf;
			if n <> (String.length buf) then failwith "Failed to transmit fd";

			let n = Unix.write local jsonrpc_request 0 (String.length jsonrpc_request) in
			Printf.fprintf stderr "sent message = [%s]\n%!" jsonrpc_request;
			Unix.shutdown local Unix.SHUTDOWN_SEND;
			if n <> (String.length jsonrpc_request) then failwith "Failed to transmit fd";
			let buf' = String.make 16384 '\000' in
			let _ = Unix.read local buf' 0 (String.length buf') in
			Printf.fprintf stderr "received [%s]\n%!" buf';
			begin match Http_client.response_of_fd local with
				| None -> failwith "Invalid HTTP response"
				| Some resp ->
					let buf' = String.make 16384 '\000' in
					let n = Unix.recv local buf' 0 (String.length buf') [] in
					if n = 0 then failwith "Received a zero-length response";
					let resp = String.sub buf' 0 n |> Jsonrpc.response_of_string in
					if not resp.Rpc.success then failwith "Received a failure";
					Printf.fprintf stderr "Received [%s]\n%!" buf';
					let resp' = Xenops_interface.string_response_of_rpc resp.Rpc.contents in
					resp' |> success |> wait_for_task rpc |> success_task rpc;
			end;
			let _ = Unix.waitpid [] pid in
			()
		) (fun () -> close a; close b)

let trim limit str =
	let l = String.length str in
	if l < limit then str
	else
		"..." ^ (String.sub str (l - limit + 3) (limit - 3))

let vbd_list x =
	let vm, _ = find_by_name x in
	let vbds = success (Client.VBD.list rpc vm.Vm.id) in
	let line id position mode ty plugged disk =
		Printf.sprintf "%-10s %-8s %-4s %-5s %-7s %s" id position mode ty plugged disk in
	let header = line "id" "position" "mode" "type" "plugged" "disk" in
	let lines = List.map
		(fun (vbd, state) ->
			let id = snd vbd.Vbd.id in
			let position = Opt.default "None" (Opt.map Device_number.to_linux_device vbd.Vbd.position) in
			let mode = if vbd.Vbd.mode = Vbd.ReadOnly then "RO" else "RW" in
			let ty = match vbd.Vbd.ty with Vbd.CDROM -> "CDROM" | Vbd.Disk -> "HDD" in
			let plugged = if state.Vbd.plugged then "X" else " " in
			let disk = Opt.default "" (Opt.map (function
				| Local x -> x |> trim 32
				| VDI path -> path |> trim 32
			) vbd.Vbd.backend) in
			line id position mode ty plugged disk
		) vbds in
	List.iter print_endline (header :: lines)

let find_vbd id =
	let vbd_id : Vbd.id = match String.split ~limit:2 '.' id with
		| [ a; b ] -> a, b
		| _ ->
			Printf.fprintf stderr "Failed to parse VBD id: %s (expected VM.device)\n" id;
			exit 1 in
	let vm_id = fst vbd_id in
	let vm, _ = find_by_name vm_id in
	let vbds = success (Client.VBD.list rpc vm.Vm.id) in
	let this_one (y, _) = snd y.Vbd.id = snd vbd_id in
	try
		List.find this_one vbds
	with Not_found ->
		Printf.fprintf stderr "Failed to find VBD: %s\n" id;
		exit 1

let cd_eject id =
	let vbd, _ = find_vbd id in
	Client.VBD.eject rpc vbd.Vbd.id |> success |> wait_for_task rpc |> success_task rpc

let cd_insert id disk =
	let vbd, _ = find_vbd id in
	Client.VBD.insert rpc vbd.Vbd.id (Local disk) |> success |> wait_for_task rpc |> success_task rpc

let slave () =
	let copy a b =
		let len = 1024 * 1024 in
		let buf = String.make len '\000' in
		let finished = ref false in
		while not !finished do
			let n = Unix.read a buf 0 len in
			if n = 0 then finished := true
			else
				let m = Unix.write b buf 0 n in
				if m <> n then finished := true
		done in

	let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
	finally
		(fun () ->
			Unix.connect fd (Unix.ADDR_UNIX default_path);
			let incoming = Thread.create (copy Unix.stdin) fd in
			let outgoing = Thread.create (copy fd) Unix.stdout in
			Thread.join incoming;
			Thread.join outgoing;
		) (fun () -> Unix.close fd)

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
		| [ "reboot"; id ] ->
			reboot id None
		| [ "reboot"; id; timeout ] ->
			reboot id (Some (float_of_string timeout))
		| [ "suspend"; id; disk ] ->
			suspend id disk
		| [ "resume"; id; disk ] ->
			resume id disk
		| "migrate" :: id :: remotecmd ->
			migrate id remotecmd
		| [ "vbd-list"; id ] ->
			vbd_list id
		| [ "cd-insert"; id; disk ] ->
			cd_insert id disk
		| [ "cd-eject"; id ] ->
			cd_eject id
		| [ "slave" ] ->
			slave ()
		| cmd :: _ ->
			Printf.fprintf stderr "Unrecognised command: %s\n" cmd;
			usage ();
			exit 1

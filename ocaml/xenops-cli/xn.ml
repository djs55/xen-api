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
open Xenops_interface
open Xenops_client

let usage () =
	Printf.fprintf stderr "%s <command> [args] - send commands to the xenops daemon\n" Sys.argv.(0);
	Printf.fprintf stderr "%s add <config> - add a VM from <config>\n" Sys.argv.(0);
	Printf.fprintf stderr "%s list - query the states of known VMs\n" Sys.argv.(0);
	Printf.fprintf stderr "%s remove <name or id> - forget about a VM\n" Sys.argv.(0);
	Printf.fprintf stderr "%s start <name or id> [paused] - start a VM\n" Sys.argv.(0);
	Printf.fprintf stderr "%s pause <name or id> - pause a VM\n" Sys.argv.(0);
	Printf.fprintf stderr "%s unpause <name or id> - unpause a VM\n" Sys.argv.(0);
	Printf.fprintf stderr "%s shutdown <name or id> - shutdown a VM\n" Sys.argv.(0);
	Printf.fprintf stderr "%s reboot <name or id> - reboot a VM\n" Sys.argv.(0);
	Printf.fprintf stderr "%s suspend <name or id> <disk> - suspend a VM\n" Sys.argv.(0);
	Printf.fprintf stderr "%s resume <name or id> <disk> - resume a VM\n" Sys.argv.(0);
	Printf.fprintf stderr "%s migrate <name or id> <url> - migrate a VM to <url>\n" Sys.argv.(0);
	Printf.fprintf stderr "%s vbd-list <name or id> - query the states of a VM's block devices\n" Sys.argv.(0);
	Printf.fprintf stderr "%s pci-add <name or id> <number> <bdf> - associate the PCI device <bdf> with <name or id>\n" Sys.argv.(0);
	Printf.fprintf stderr "%s pci-remove <name or id> <number> - disassociate the PCI device <bdf> with <name or id>\n" Sys.argv.(0);
	Printf.fprintf stderr "%s pci-list <name or id> - query the states of a VM's PCI devices\n" Sys.argv.(0);
	Printf.fprintf stderr "%s cd-insert <id> <disk> - insert a CD into a VBD\n" Sys.argv.(0);
	Printf.fprintf stderr "%s cd-eject <id> - eject a CD from a VBD\n" Sys.argv.(0);
	Printf.fprintf stderr "%s export-metadata <id> - export metadata associated with <id>\n" Sys.argv.(0);
	Printf.fprintf stderr "%s export-metadata-xm <id> - export metadata associated with <id> in xm format\n" Sys.argv.(0);
	()

let success_task id =
	let t = Client.TASK.stat id |> success in
	match t.Task.result with
	| Task.Completed _ -> t
	| Task.Failed (Failed_to_contact_remote_service x) ->
		Printf.printf "Failed to contact remote service on: %s\n" x;
		Printf.printf "Check the address and credentials.\n";
		exit 1;
	| Task.Failed x -> failwith (Jsonrpc.to_string (rpc_of_error x))
	| Task.Pending _ -> failwith "task pending"

let parse_source x = match List.filter (fun x -> x <> "") (String.split ':' x) with
	| [ "phy"; path ] -> Some (Local path)
	| [ "sm"; path ] -> Some (VDI path)
	| [ "file"; path ] ->
		Printf.fprintf stderr "I don't understand 'file' disk paths. Please use 'phy'.\n";
		exit 2
	| [] -> None (* empty *)
	| _ ->
		Printf.fprintf stderr "I don't understand '%s'. Please use 'phy:path,...\n" x;
		exit 2

let print_source = function
	| None -> ""
	| Some (Local path) -> Printf.sprintf "phy:%s" path
	| Some (VDI path) -> Printf.sprintf "sm:%s" path

let print_pci x =
	let open Pci in
	let open Xn_cfg_types in
	Printf.sprintf "%04x:%02x:%02x.%01x,%s=%d,%s=%d" x.domain x.bus x.dev x.fn
		_msitranslate (if x.msitranslate then 1 else 0)
		_power_mgmt (if x.power_mgmt then 1 else 0)

let parse_pci vm_id (x, idx) = match String.split ',' x with
	| bdf :: options ->
		let hex x = int_of_string ("0x" ^ x) in
		let parse_dev_fn x = match String.split '.' x with
			| [ dev; fn ] -> hex dev, hex fn
			| _ ->
				Printf.fprintf stderr "Failed to parse BDF: %s. It should be '[DDDD:]BB:VV.F'\n" bdf;
				exit 2 in
		let domain, bus, dev, fn = match String.split ':' bdf with
			| [ domain; bus; dev_dot_fn ] ->
				let dev, fn = parse_dev_fn dev_dot_fn in
				hex domain, hex bus, dev, fn
			| [ bus; dev_dot_fn ] ->
				let dev, fn = parse_dev_fn dev_dot_fn in
				0, hex bus, dev, fn
			| _ ->
				Printf.fprintf stderr "Failed to parse BDF: %s. It should be '[DDDD:]BB:VV.F'\n" bdf;
				exit 2 in
		let options = List.map (fun x -> match String.split ~limit:2 '=' x with
			| [k; v] -> k, v
			| _ ->
				Printf.fprintf stderr "Failed to parse PCI option: %s. It should be key=value.\n" x;
				exit 2
		) options in
		let default_bool d k opts =
			if List.mem_assoc k opts then List.assoc k opts = "1" else d in
		let open Pci in
		let open Xn_cfg_types in
		let msitranslate = default_bool false _msitranslate options in
		let power_mgmt = default_bool false _power_mgmt options in
		{
			Pci.id = vm_id, string_of_int idx;
			domain = domain;
			bus = bus;
			dev = dev;
			fn = fn;
			msitranslate = msitranslate;
			power_mgmt = power_mgmt
		}
	| _ ->
		Printf.fprintf stderr "Failed to parse PCI '%s'. It should be '[DDDD:]BB:VV.F[,option1[,option2]]'." x;
		exit 2

let print_disk vbd =
	let device_number = snd vbd.Vbd.id in
	let mode = match vbd.Vbd.mode with
		| Vbd.ReadOnly -> "r"
		| Vbd.ReadWrite -> "w" in
	let ty = match vbd.Vbd.ty with
		| Vbd.CDROM -> ":cdrom"
		| Vbd.Disk -> "" in
	let source = print_source vbd.Vbd.backend in
	Printf.sprintf "%s,%s%s,%s" source device_number ty mode

let parse_disk vm_id x = match String.split ',' x with
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
		let backend = parse_source source in
		{
			Vbd.id = vm_id, device_number;
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
		exit 2

let print_disk vbd =
	let device_number = snd vbd.Vbd.id in
	let mode = match vbd.Vbd.mode with
		| Vbd.ReadOnly -> "r"
		| Vbd.ReadWrite -> "w" in
	let ty = match vbd.Vbd.ty with
		| Vbd.CDROM -> ":cdrom"
		| Vbd.Disk -> "" in
	let source = print_source vbd.Vbd.backend in
	Printf.sprintf "%s,%s%s,%s" source device_number ty mode

let print_vif vif =
	let mac = if vif.Vif.mac = "" then "" else Printf.sprintf "mac=%s" vif.Vif.mac in
	let bridge = match vif.Vif.backend with
		| Bridge x -> Printf.sprintf "bridge=%s" x
		| VSwitch x -> Printf.sprintf "bridge=%s" x
		| Netback (_, _) ->
			Printf.fprintf stderr "Cannot handle backend = Netback(_, _)\n%!";
			exit 2 in
	String.concat "," [ mac; bridge ]

let parse_vif vm_id (x, idx) =
	let open Xn_cfg_types in
	let xs = List.filter (fun x -> x <> "") (List.map (String.strip String.isspace) (String.split ',' x)) in
	let kvpairs = List.map (fun x -> match String.split ~limit:2 '=' x with
		| [ k; v ] -> k, v
		| _ ->
			Printf.fprintf stderr "I don't understand '%s'. Please use 'mac=xx:xx:xx:xx:xx:xx,bridge=xenbrX'.\n" x;
			exit 2
	) xs in {
		Vif.id = vm_id, string_of_int idx;
		position = idx;
		mac = if List.mem_assoc _mac kvpairs then List.assoc _mac kvpairs else "";
		carrier = true;
		mtu = 1500;
		rate = None;
		backend = if List.mem_assoc _bridge kvpairs then VSwitch (List.assoc _bridge kvpairs) else VSwitch "xenbr0";
		other_config = [];
		extra_private_keys = [];
	}

let print_vm id =
	let open Xn_cfg_types in
	let open Vm in
	let vm_t, _ = Client.VM.stat id |> success in
	let boot = match vm_t.ty with
		| PV { boot = boot } ->
			begin match boot with
				| Direct { kernel = k; cmdline = c; ramdisk = i } -> [
					_builder, "linux";
					_kernel, k;
					_root, c
				] @ (Opt.default [] (Opt.map (fun x -> [ _ramdisk, x ]) i))
				| Indirect { bootloader = b } -> [
					_builder, "linux";
					_bootloader, b;
				]
			end
		| HVM { boot_order = b } -> [
			_builder, "hvmloader";
			_boot, b
		] in
	let vcpus = [ _vcpus, string_of_int vm_t.vcpus ] in
	let bytes_to_mib x = Int64.div x (Int64.mul 1024L 1024L) in
	let memory = [ _memory, vm_t.memory_static_max |> bytes_to_mib |> Int64.to_string ] in
	let vbds = Client.VBD.list id |> success |> List.map fst in
	let vbds = [ _disk, Printf.sprintf "[ %s ]" (String.concat ", " (List.map (fun x -> Printf.sprintf "'%s'" (print_disk x)) vbds)) ] in
	let vifs = Client.VIF.list id |> success |> List.map fst in
	let vifs = [ _vif, Printf.sprintf "[ %s ]" (String.concat ", " (List.map (fun x -> Printf.sprintf "'%s'" (print_vif x)) vifs)) ] in
	let pcis = Client.PCI.list id |> success |> List.map fst in
	(* Sort into order based on id *)
	let pcis = List.sort (fun a b -> compare a.Pci.id b.Pci.id) pcis in
	let pcis = [ _pci, Printf.sprintf "[ %s ]" (String.concat ", " (List.map (fun x -> Printf.sprintf "'%s'" (print_pci x)) pcis)) ] in
	String.concat "\n" (List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v)
		(boot @ vcpus @ memory @ vbds @ vifs @ pcis))


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
			let (id: Vm.id) = success (Client.VM.add vm) in
			let disks = if mem _disk then find _disk |> list string else [] in
			let one x = x |> parse_disk id |> Client.VBD.add |> success in
			let (_: Vbd.id list) = List.map one disks in
			let vifs = if mem _vif then find _vif |> list string else [] in
			let vifs = List.combine vifs (Range.to_list (Range.make 0 (List.length vifs))) in
			let one x = x |> parse_vif id |> Client.VIF.add |> success in
			let (_: Vif.id list) = List.map one vifs in
			let pcis = if mem _pci then find _pci |> list string else [] in
			let pcis = List.combine pcis (Range.to_list (Range.make 0 (List.length pcis))) in
			let one x = x |> parse_pci id |> Client.PCI.add |> success in
			let (_: Pci.id list) = List.map one pcis in
			Printf.printf "%s\n" id
		)

let list () =
	let open Vm in
	let line name domid mem vcpus state time =
		Printf.sprintf "%-45s%-5s%-6s%-5s     %-8s%-s" name domid mem vcpus state time in
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
	let vms = success (Client.VM.list ()) in
	let lines = header :: (List.map string_of_vm vms) in
	List.iter (Printf.printf "%s\n") lines

let find_by_name x =
	let open Vm in
	let all = success (Client.VM.list ()) in
	let this_one (y, _) = y.id = x || y.name = x in
	try
		List.find this_one all
	with Not_found ->
		Printf.fprintf stderr "Failed to find VM: %s\n" x;
		exit 1

let remove x =
	let open Vm in
	let vm, _ = find_by_name x in
	let vbds = success (Client.VBD.list vm.id) in
	List.iter
		(fun (vbd, _) ->
			success (Client.VBD.remove vbd.Vbd.id)
		) vbds;
	let vifs = success (Client.VIF.list vm.id) in
	List.iter
		(fun (vif, _) ->
			success (Client.VIF.remove vif.Vif.id)
		) vifs;
	success (Client.VM.remove vm.id)

let export_metadata x filename =
	let open Vm in
	let vm, _ = find_by_name x in
	let txt = Client.VM.export_metadata vm.id |> success in
	Unixext.write_string_to_file filename txt

let export_metadata_xm x filename =
	let open Vm in
	let vm, _ = find_by_name x in
	let txt = print_vm vm.id in
	Unixext.write_string_to_file filename txt

let import_metadata filename =
	let txt = Unixext.string_of_file filename in
	let id = Client.VM.import_metadata txt |> success in
	Printf.printf "%s\n" id

let start x paused =
	let open Vm in
	let vm, _ = find_by_name x in
	Client.VM.start vm.id |> success |> wait_for_task |> success_task |> ignore_task;
	if not paused
	then Client.VM.unpause vm.id |> success |> wait_for_task |> success_task |> ignore_task

let shutdown x =
	let open Vm in
	let vm, _ = find_by_name x in
	Client.VM.shutdown vm.id |> success |> wait_for_task |> success_task

let pause x =
	let open Vm in
	let vm, _ = find_by_name x in
	Client.VM.pause vm.id |> success |> wait_for_task |> success_task

let unpause x =
	let open Vm in
	let vm, _ = find_by_name x in
	Client.VM.unpause vm.id |> success |> wait_for_task |> success_task

let reboot x timeout =
	let open Vm in
	let vm, _ = find_by_name x in
	Client.VM.reboot vm.id timeout |> success |> wait_for_task |> success_task

let suspend x disk =
	let open Vm in
	let vm, _ = find_by_name x in
	Client.VM.suspend vm.id (Local disk) |> success |> wait_for_task |> success_task

let resume x disk =
	let open Vm in
	let vm, _ = find_by_name x in
	Client.VM.resume vm.id (Local disk) |> success |> wait_for_task |> success_task

let migrate x url =
	let open Vm in
	let vm, _ = find_by_name x in
	Client.VM.migrate vm.id url |> success |> wait_for_task |> success_task

let trim limit str =
	let l = String.length str in
	if l < limit then str
	else
		"..." ^ (String.sub str (l - limit + 3) (limit - 3))

let vbd_list x =
	let vm, _ = find_by_name x in
	let vbds = success (Client.VBD.list vm.Vm.id) in
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

let pci_add x idx bdf =
	let vm, _ = find_by_name x in
	let open Pci in
	let domain, bus, dev, fn = Scanf.sscanf bdf "%04x:%02x:%02x.%1x" (fun a b c d -> a, b, c, d) in
	let id = Client.PCI.add {
		id = (vm.Vm.id, idx);
		domain = domain;
		bus = bus;
		dev = dev;
		fn = fn;
		msitranslate = false;
		power_mgmt = false
	} |> success in
	Printf.printf "%s.%s\n" (fst id) (snd id)

let pci_remove x idx =
	let vm, _ = find_by_name x in
	Client.PCI.remove (vm.Vm.id, idx) |> success

let pci_list x =
	let vm, _ = find_by_name x in
	let pcis = success (Client.PCI.list vm.Vm.id) in
	let line id bdf =
		Printf.sprintf "%-10s %-12s" id bdf in
	let header = line "id" "bdf" in
	let lines = List.map
		(fun (pci, state) ->
			let open Pci in
			let id = snd pci.id in
			let bdf = Printf.sprintf "%04x:%02x:%02x.%01x" pci.domain pci.bus pci.dev pci.fn in
			line id bdf
		) pcis in
	List.iter print_endline (header :: lines)

let find_vbd id =
	let vbd_id : Vbd.id = match String.split ~limit:2 '.' id with
		| [ a; b ] -> a, b
		| _ ->
			Printf.fprintf stderr "Failed to parse VBD id: %s (expected VM.device)\n" id;
			exit 1 in
	let vm_id = fst vbd_id in
	let vm, _ = find_by_name vm_id in
	let vbds = success (Client.VBD.list vm.Vm.id) in
	let this_one (y, _) = snd y.Vbd.id = snd vbd_id in
	try
		List.find this_one vbds
	with Not_found ->
		Printf.fprintf stderr "Failed to find VBD: %s\n" id;
		exit 1

let cd_eject id =
	let vbd, _ = find_vbd id in
	Client.VBD.eject vbd.Vbd.id |> success |> wait_for_task |> success_task

let cd_insert id disk =
	let vbd, _ = find_vbd id in
	let backend = parse_source disk |> Opt.unbox in
	Client.VBD.insert vbd.Vbd.id backend |> success |> wait_for_task |> success_task

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

let verbose_task t =
	let string_of_result = function
		| Task.Completed t -> Printf.sprintf "%.2f" t
		| Task.Failed x -> Printf.sprintf "Error: %s" (x |>rpc_of_error |> Jsonrpc.to_string)
		| Task.Pending _ -> Printf.sprintf "Error: still pending" in
	let rows = List.map (fun (name, result) -> [ name;  string_of_result result ]) t.Task.subtasks in
	Table.print rows;
	Printf.printf "\n";
	Printf.printf "Overall: %s\n" (string_of_result t.Task.result)


let _ =
	let args = Sys.argv |> Array.to_list |> List.tl in
	let verbose = List.mem "-v" args in
	let args = List.filter (fun x -> x <> "-v") args in
	let task = if verbose then verbose_task else ignore_task in
	match args with
		| [ "help" ] | [] ->
			usage ();
			exit 0
		| [ "add"; filename ] ->
			add filename
		| [ "list" ] ->
			list ()
		| [ "remove"; id ] ->
			remove id
		| [ "export-metadata"; id; filename ] ->
			export_metadata id filename
		| [ "export-metadata-xm"; id; filename ] ->
			export_metadata_xm id filename
		| [ "import-metadata"; filename ] ->
			import_metadata filename
		| [ "start"; id; "paused" ] ->
			start id true
		| [ "start"; id ] ->
			start id false
		| [ "pause"; id ] ->
			pause id |> task
		| [ "unpause"; id ] ->
			unpause id |> task
		| [ "shutdown"; id ] ->
			shutdown id |> task
		| [ "reboot"; id ] ->
			reboot id None |> task
		| [ "reboot"; id; timeout ] ->
			reboot id (Some (float_of_string timeout)) |> task
		| [ "suspend"; id; disk ] ->
			suspend id disk |> task
		| [ "resume"; id; disk ] ->
			resume id disk |> task
		| [ "migrate"; id; url ] ->
			migrate id url |> task
		| [ "vbd-list"; id ] ->
			vbd_list id
		| [ "pci-add"; id; idx; bdf ] ->
			pci_add id idx bdf
		| [ "pci-remove"; id; idx] ->
			pci_remove id idx
		| [ "pci-list"; id ] ->
			pci_list id
		| [ "cd-insert"; id; disk ] ->
			cd_insert id disk |> task
		| [ "cd-eject"; id ] ->
			cd_eject id |> task
		| [ "slave" ] ->
			slave ()
		| cmd :: _ ->
			Printf.fprintf stderr "Unrecognised command: %s\n" cmd;
			usage ();
			exit 1

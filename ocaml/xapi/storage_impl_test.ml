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
 * @group Storage
 *)

open Pervasiveext
open Threadext
open Storage_interface

let total_errors = ref 0
let total_errors_m = Mutex.create ()

let debug = Storage_impl.debug
let error = Storage_impl.error
let info = Storage_impl.info

let inc_errors () =
	Mutex.execute total_errors_m
		(fun () ->
			incr total_errors
		)

exception Api_error of string * (string list)

module Debug_print_impl = struct
	type context = unit
	module DP = struct
		let create context ~task ~id = assert false
		let destroy context ~task ~dp = assert false
		let diagnostics context () = assert false
	end
	module VDI = struct
		let m = Mutex.create ()
		let attached = Hashtbl.create 10
		let activated = Hashtbl.create 10
		let key_of sr vdi = Printf.sprintf "%s/%s" sr vdi

		let stat context ~task ?dp ~sr ~vdi () = assert false

		let create context ~task ~sr ~name_label ~name_description ~virtual_size ~ty ~params =
			if List.mem_assoc "toosmall" params
			then Success (NewVdi { vdi = "newvdi"; virtual_size = Int64.sub virtual_size 1L })
			else Success (NewVdi { vdi = "newvdi"; virtual_size = virtual_size })

		let attach context ~task ~dp ~sr ~vdi ~read_write =
			info "VDI.attach dp:%s sr:%s vdi:%s read_write:%b" dp sr vdi read_write;
			if dp = "error"
			then raise (Api_error("SR_BACKEND_FAILURE_test", [ "this"; "is"; "an"; "example" ]));
			if dp = "error2"
			then Failure (Backend_error ("SR_BACKEND_FAILURE_test", [ "this"; "is"; "an"; "example" ]))
			else begin
				Mutex.execute m
					(fun () ->
						let key = key_of sr vdi in
						if Hashtbl.mem attached key then begin
							inc_errors ();
						error "VDI.attach dp:%s sr:%s vdi:%s : double attach" dp sr vdi;
						failwith "double attach"
						end else Hashtbl.replace attached key ());
				Success (Storage_interface.Vdi { Storage_interface.backend_domain = None; physical_device = "XXX" })
			end
		let activate context ~task ~dp ~sr ~vdi =
			Mutex.execute m
				(fun () ->
					let key = key_of sr vdi in
					if Hashtbl.mem activated key then begin
						inc_errors ();
						error "VDI.detach dp:%s sr:%s vdi:%s : double activate" dp sr vdi;
						failwith "double activate"
					end else Hashtbl.replace activated key ());
			info "VDI.activate dp:%s sr:%s vdi:%s" dp sr vdi;
			Success Unit

		let working = ref false

		let detach context ~task ~dp ~sr ~vdi =
			if vdi = "error" && not(!working)
			then raise (Api_error("SR_BACKEND_FAILURE_test", [ "this"; "is"; "an"; "example" ]));
			if vdi = "error2" && not(!working)
			then Failure (Backend_error ("SR_BACKEND_FAILURE_test", [ "this"; "is"; "an"; "example" ]))
			else begin
				Mutex.execute m
					(fun () ->
						let key = key_of sr vdi in
						if not (Hashtbl.mem attached key) then begin
							inc_errors ();
							error "VDI.detach dp:%s sr:%s vdi:%s : double detach" dp sr vdi;
							failwith "double detach"
						end else Hashtbl.remove attached key);
				info "VDI.detach dp:%s sr:%s vdi:%s" dp sr vdi;
				Success Unit
			end
		let deactivate context ~task ~dp ~sr ~vdi =
			Mutex.execute m
				(fun () ->
					let key = key_of sr vdi in
					if not (Hashtbl.mem activated key) then begin
						inc_errors ();
						error "VDI.deactivate dp:%s sr:%s vdi:%s : double deactivate" dp sr vdi;
						failwith "double deactivate"
					end else Hashtbl.remove activated key);
			info "VDI.deactivate dp:%s sr:%s vdi:%s" dp sr vdi;
			Success Unit
	end

	module SR = struct
		let list context ~task = assert false
		let attach context ~task ~sr =
			info "SR.attach sr:%s" sr;
			Success Unit
		let fail_if_anything_leaked () = 
			Mutex.execute VDI.m
				(fun () ->
					Hashtbl.iter
						(fun k _ ->
							error "leaked attach: %s" k;
							inc_errors ();
						) VDI.attached;
					Hashtbl.iter
						(fun k _ ->
							error "leaked activate: %s" k;
							inc_errors ();
						) VDI.activated
				)
		let detach context ~task ~sr =
			info "SR.detach sr:%s" sr;
			fail_if_anything_leaked ();
			Success Unit
		let destroy context ~task ~sr =
			info "SR.destroy sr:%s" sr;
			fail_if_anything_leaked (); 
			Success Unit
	end

end


module Server=Server(Storage_impl.Wrapper(Debug_print_impl))

let path = "/tmp/storage"

let rpc_unix call = Rpc_client.do_rpc_unix ~version:"1.0" ~filename:path ~path:"/" call
let rpc_inprocess call = Server.process () call

let use_inprocess_rpc = ref true

let rpc call = if !use_inprocess_rpc then rpc_inprocess call else rpc_unix call

let task = "task"

let datapath_of_id id = Client.DP.create rpc ~task ~id

let expect expected f x =
	if not(f x) then begin
		error "wanted %s; got:%s" expected (string_of_result x);
		inc_errors ();
	end

let test_backend_error = function
	| Failure (Backend_error(code, params)) when code = "SR_BACKEND_FAILURE_test" -> true
	| _ -> false

let too_small_backend_error = function
	| Failure (Backend_error(code, params)) when code = "SR_BACKEND_FAILURE" && (List.hd params = "Disk too small") -> true
	| _ -> false

let internal_error = function
	| Failure (Internal_error "Storage_impl_test.Api_error(\"SR_BACKEND_FAILURE_test\", _)") -> true
	| _ -> false

let test_vdis sr : unit =
	let num_users = 10 in
	let num_vdis = 10 in
	let iterations = 10 in
	let one id sr vdi () =
		let dp = datapath_of_id id in
		for i = 0 to iterations - 1 do
			expect "Vdi _" (function Success (Vdi _) -> true | _ -> false)
				(Client.VDI.attach rpc ~task ~dp ~sr ~vdi ~read_write:false);
			expect "Attached(RO)" (function Success (State (Vdi_automaton.Attached Vdi_automaton.RO)) -> true | _ -> false)
				(Client.VDI.stat rpc ~task ~dp ~sr ~vdi ());
			expect "()" (fun x -> x = Success Unit)
				(Client.VDI.detach rpc ~task ~dp ~sr ~vdi);
			expect "Detached" (function Success (State (Vdi_automaton.Detached)) -> true | _ -> false)
				(Client.VDI.stat rpc ~task ~dp ~sr ~vdi ());
			expect "()" (fun x -> x = Success Unit)
				(Client.VDI.detach rpc ~task ~dp ~sr ~vdi);
			expect "Vdi _" (function Success (Vdi _) -> true | _ -> false)
				(Client.VDI.attach rpc ~task ~dp ~sr ~vdi ~read_write:false);
			expect "Attached(RO)" (function Success (State (Vdi_automaton.Attached Vdi_automaton.RO)) -> true | _ -> false)
				(Client.VDI.stat rpc ~task ~dp ~sr ~vdi ());
			expect "()" (fun x -> x = Success Unit)
				(Client.VDI.activate rpc ~task ~dp ~sr ~vdi);
			expect "Activated(RO)" (function Success (State (Vdi_automaton.Activated Vdi_automaton.RO)) -> true | _ -> false)
				(Client.VDI.stat rpc ~task ~dp ~sr ~vdi ());
			expect "()" (fun x -> x = Success Unit)
				(Client.VDI.deactivate rpc ~task ~dp ~sr ~vdi);
			expect "Attached(RO)" (function Success (State (Vdi_automaton.Attached Vdi_automaton.RO)) -> true | _ -> false)
				(Client.VDI.stat rpc ~task ~dp ~sr ~vdi ());
			expect "()" (fun x -> x = Success Unit)
				(Client.VDI.detach rpc ~task ~dp ~sr ~vdi);
			expect "Detached" (function (Success (State (Vdi_automaton.Detached))) -> true | _ -> false)
				(Client.VDI.stat rpc ~task ~dp ~sr ~vdi ());
		done in
	let vdis = Range.to_list (Range.make 0 num_vdis) in
	let users = Range.to_list (Range.make 0 num_users) in
	let bodies =
		List.concat (
			List.map (fun user ->
				List.map (fun vdi -> one (Printf.sprintf "vdi:%d/user:%d" vdi user) sr (string_of_int vdi)) vdis
			) users
		) in
	info "Starting %d threads%!" (List.length bodies);
	let threads = List.map (fun f -> Thread.create f ()) bodies in
	info "Joining %d threads%!" (List.length bodies);
	List.iter Thread.join threads

let leak dp sr activate vdi =
	info "Leaking some attaches and activates";
	expect "Vdi _" (function Success (Vdi _) -> true | _ -> false)
		(Client.VDI.attach rpc ~task ~dp ~sr ~vdi ~read_write:true);
	if activate
	then expect "()" (function Success Unit -> true | _ -> false)
		(Client.VDI.activate rpc ~task ~dp ~sr ~vdi)

let test_sr sr =
	let dp = datapath_of_id "pbd" in
	expect "()" (fun x -> x = Success Unit)
		(Client.SR.attach rpc ~task ~sr);
	test_vdis sr;
	leak dp sr false "leaked";
	leak dp sr true "leaked2";
	info "About to SR.detach";
	expect "()" (fun x -> x = Success Unit)
		(Client.SR.detach rpc ~task ~sr);
	expect "()" (fun x -> x = Success Unit)
		(Client.SR.attach rpc ~task ~sr);
	leak dp sr false "leaked";
	leak dp sr true "leaked2";
	info "About to logout";
	expect "()" (fun x -> x = Success Unit)
		(Client.DP.destroy rpc ~task ~dp ~allow_leak:false);
	info "About to SR.detach";
	expect "()" (function Success Unit -> true | _ -> false)
		(Client.SR.detach rpc ~task ~sr);
	(* About to check the error handling *)
	let dp = datapath_of_id "error" in
	expect "()" (fun x -> x = Success Unit)
		(Client.SR.attach rpc ~task ~sr);
	debug "This VDI.attach should fail:";
	expect "internal_error" internal_error
		(Client.VDI.attach rpc ~task ~dp ~sr ~vdi:"leaked" ~read_write:true);
	let dp = datapath_of_id "error2" in
	debug "This VDI.attach should fail:";
	expect "test_backend_error" test_backend_error
		(Client.VDI.attach rpc ~task ~dp ~sr ~vdi:"leaked" ~read_write:true);
	debug "Detaching and cleaning up";
	expect "()" (fun x -> x = Success Unit)
		(Client.SR.detach rpc ~task ~sr)

(* Check the VDI.stat function works *)
let test_stat sr vdi = 
	expect "()" (fun x -> x = Success Unit)
		(Client.SR.attach rpc ~task ~sr);
	let dp1 = datapath_of_id "dp1" (* will be r/w *)
	and dp2 = datapath_of_id "dp2" (* will be r/o *) in
	expect "Vdi _" (function Success (Vdi _) -> true | _ -> false)
		(Client.VDI.attach rpc ~task ~dp:dp1 ~sr ~vdi ~read_write:true);
	(* dp1: Attached(RW) dp2: Detached superstate: Attached(RW) *)
	expect "Attached(RW)" (function Success (State (Vdi_automaton.Attached Vdi_automaton.RW)) -> true | _ -> false)
		(Client.VDI.stat rpc ~task ~dp:dp1 ~sr ~vdi ());
	expect "Attached(RW)" (function Success (State (Vdi_automaton.Attached Vdi_automaton.RW)) -> true | _ -> false)
		(Client.VDI.stat rpc ~task ~sr ~vdi ());
	expect "Vdi _" (function Success (Vdi _) -> true | _ -> false)
		(Client.VDI.attach rpc ~task ~dp:dp2 ~sr ~vdi ~read_write:false);
	(* dp1: Attached(RW) dp2: Attached(RO) superstate: Attached(RW) *)
	expect "Attached(RO)" (function Success (State (Vdi_automaton.Attached Vdi_automaton.RO)) -> true | _ -> false)
		(Client.VDI.stat rpc ~task ~dp:dp2 ~sr ~vdi ());
	expect "Attached(RW)" (function Success (State (Vdi_automaton.Attached Vdi_automaton.RW)) -> true | _ -> false)
		(Client.VDI.stat rpc ~task ~sr ~vdi ());
	expect "Illegal transition" (fun x -> x = Failure(Illegal_transition(Vdi_automaton.Attached(Vdi_automaton.RW), Vdi_automaton.Attached(Vdi_automaton.RO))))
		(Client.VDI.detach rpc ~task ~dp:dp1 ~sr ~vdi);
	expect "()" (fun x -> x = Success Unit)
		(Client.VDI.detach rpc ~task ~dp:dp2 ~sr ~vdi);
	(* dp1: Attached(RW) dp2: Detached superstate: Attached(RW) *)
	expect "Detached" (function Success (State Vdi_automaton.Detached) -> true | _ -> false)
		(Client.VDI.stat rpc ~task ~dp:dp2 ~sr ~vdi ());
	expect "Attached(RW)" (function Success (State (Vdi_automaton.Attached Vdi_automaton.RW)) -> true | _ -> false)
		(Client.VDI.stat rpc ~task ~sr ~vdi ());
	expect "()" (fun x -> x = Success Unit)
		(Client.VDI.detach rpc ~task ~dp:dp1 ~sr ~vdi);
	(* dp1: Detached dp1: Detached superstate: Detached *)
	expect "Detached" (function Success (State Vdi_automaton.Detached) -> true | _ -> false)
		(Client.VDI.stat rpc ~task ~sr ~vdi ());
	expect "()" (fun x -> x = Success Unit)
		(Client.SR.detach rpc ~task ~sr)

(* Manual cleanup with VDI.detach (fails) and then SR.detach (succeeds) *)
let test_sr_detach_cleanup_errors_1 sr vdi =
	Debug_print_impl.VDI.working := false;
	expect "()" (fun x -> x = Success Unit)
		(Client.SR.attach rpc ~task ~sr);
	let dp = datapath_of_id "datapath" in
	leak dp sr true vdi;
	expect "()" (fun x -> x = Success Unit)
		(Client.VDI.deactivate rpc ~task ~dp ~sr ~vdi);
	if vdi = "error2"
	then expect "test_backend_error" test_backend_error
		(Client.VDI.detach rpc ~task ~dp ~sr ~vdi)
	else expect "internal_error" internal_error
		(Client.VDI.detach rpc ~task ~dp ~sr ~vdi);
	debug "Detaching and cleaning up";
	Debug_print_impl.VDI.working := true;
	(* Should succeed because the VDI.attach state will have been forgotten: FH2 *)
	expect "()" (fun x -> x = Success Unit)
		(Client.SR.detach rpc ~task ~sr)

let test_sr_detach_cleanup_errors_2 sr vdi =
	Debug_print_impl.VDI.working := false;
	expect "()" (fun x -> x = Success Unit)
		(Client.SR.attach rpc ~task ~sr);
	let dp = datapath_of_id "datapath" in
	leak dp sr true vdi;
	if vdi = "error2"
	then expect "test_backend_error" test_backend_error
		(Client.DP.destroy rpc ~task ~dp ~allow_leak:false)
	else expect "internal_error" internal_error
		(Client.DP.destroy rpc ~task ~dp ~allow_leak:false);
	Debug_print_impl.VDI.working := true;
	debug "Attempting to attach RO (having failed a detach of a RW detach)";
	expect "Vdi _" (function Success (Vdi _) -> true | _ -> false)
		(Client.VDI.attach rpc ~task ~dp ~sr ~vdi ~read_write:false);
	debug "Detaching and cleaning up";
	expect "()" (fun x -> x = Success Unit)
		(Client.SR.detach rpc ~task ~sr)

let create_vdi_test sr = 
	expect "()" (fun x -> x = Success Unit)
		(Client.SR.attach rpc ~task ~sr);
	let name_label = "name_label" and name_description = "name_description" in
	let virtual_size = 10L and ty = "user" in
	expect "too_small_backend_error" too_small_backend_error
		(Client.VDI.create rpc ~task ~sr ~name_label ~name_description ~virtual_size ~ty ~params:["toosmall", ""]);
	expect "NewVdi" (function Success (NewVdi _) -> true | _ -> false)
		(Client.VDI.create rpc ~task ~sr ~name_label ~name_description ~virtual_size ~ty ~params:[]);
	debug "Detaching and cleaning up";
	expect "()" (fun x -> x = Success Unit)
		(Client.SR.detach rpc ~task ~sr)		

let _ =
	Storage_impl.print_debug := true;
	Storage_impl.host_state_path := "/tmp/storage.db";
	Vdi_automaton.test ();
	Unixext.unlink_safe !Storage_impl.host_state_path;
	Storage_impl.Local_domain_socket.start path Server.process;
	info "Listening on %s" Storage_impl.Local_domain_socket.path;

	test_sr "sr";

	test_sr_detach_cleanup_errors_1 "sr" "error2";
	test_sr_detach_cleanup_errors_1 "sr" "error";

	test_sr_detach_cleanup_errors_2 "sr" "error2";
	test_sr_detach_cleanup_errors_2 "sr" "error";

	test_stat "sr" "vdi";

	create_vdi_test "sr";

	if !total_errors = 0 then begin
		info "OK";
		exit 0;
	end else begin
		info "%d errors detected" !total_errors;
		exit 1
	end


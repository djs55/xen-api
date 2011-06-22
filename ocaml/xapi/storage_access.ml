(*
 * Copyright (C) 2006-2011 Citrix Systems Inc.
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
open Threadext
module XenAPI = Client.Client
open Storage_interface

module D=Debug.Debugger(struct let name="storage_access" end)
open D

module Builtin_impl = struct
	(** xapi's builtin ability to call local SM plugins using the existing
	    protocol. The code here should only call the SM functions and encapsulate
	    the return or error properly. It should not perform side-effects on
	    the xapi database: these should be handled in the layer above so they
	    can be shared with other SM implementation types.

	    Where this layer has to perform interface adjustments (see VDI.activate
	    and the read/write debacle), this highlights desirable improvements to
	    the backend interface.
	*)

	type context = unit

	let query context () = {
		name = "SMAPIv1 adapter";
		vendor = "XCP";
		version = "0.1";
		features = [];
	}

	module DP = struct
		let create context ~task ~id = assert false
		let destroy context ~task ~dp = assert false
		let diagnostics context () = assert false
	end

	module SR = struct
		let attach context ~task ~sr =
			let self = Ref.of_string sr in
			Server_helpers.exec_with_new_task "SR.attach" ~subtask_of:(Ref.of_string task)
				(fun __context ->
					Sm.call_sm_functions ~__context ~sR:self
						(fun device_config _type ->
							try
								Sm.sr_attach device_config _type self;
								Success Unit
							with e ->
								let e' = ExnHelper.string_of_exn e in
								error "SR.attach failed SR:%s error:%s" sr e';
								Failure (Internal_error e')
						)
				)
		let detach context ~task ~sr =
			let self = Ref.of_string sr in
			Server_helpers.exec_with_new_task "SR.detach" ~subtask_of:(Ref.of_string task)
				(fun __context ->
					Sm.call_sm_functions ~__context ~sR:self
						(fun device_config _type ->
							try
								Sm.sr_detach device_config _type self;
								Success Unit
							with e ->
								let e' = ExnHelper.string_of_exn e in
								error "SR.detach failed SR:%s error:%s" sr e';
								Failure (Storage_interface.Internal_error e')
						)
				)

		let destroy context ~task ~sr = 
			let self = Ref.of_string sr in
			Server_helpers.exec_with_new_task "SR.destroy" ~subtask_of:(Ref.of_string task)
				(fun __context ->
					Sm.call_sm_functions ~__context ~sR:self
						(fun device_config _type ->
							try
								Sm.sr_detach device_config _type self;
								Success Unit
							with
								| Smint.Not_implemented_in_backend ->
									Failure (Storage_interface.Backend_error(Api_errors.sr_operation_not_supported, [ sr ]))
								| e ->
									let e' = ExnHelper.string_of_exn e in
									error "SR.detach failed SR:%s error:%s" sr e';
									Failure (Storage_interface.Internal_error e')
						)
				)

		let list context ~task = assert false

	end

	module VDI = struct
		let for_vdi ~task ~sr ~vdi op_name f =
			let self = Ref.of_string vdi in
			Server_helpers.exec_with_new_task op_name ~subtask_of:(Ref.of_string task)
				(fun __context ->
					Sm.call_sm_vdi_functions ~__context ~vdi:self
						(fun device_config _type sr ->
							f device_config _type sr self
						)
				)
		
		(* Allow us to remember whether a VDI is attached read/only or read/write.
		   If this is meaningful to the backend then this should be recorded there! *)
		let vdi_read_write = Hashtbl.create 10
		let vdi_read_write_m = Mutex.create ()


		let attach context ~task ~dp ~sr ~vdi ~read_write =
			try
				let params =
					for_vdi ~task ~sr ~vdi "VDI.attach"
						(fun device_config _type sr self ->
							Sm.vdi_attach device_config _type sr self read_write
						) in
				let xenstore_keys = [ "params", params ] in
				Mutex.execute vdi_read_write_m
					(fun () -> Hashtbl.replace vdi_read_write (sr, vdi) read_write);
				Success (Vdi { backend_domain = Some 0; xenstore_keys = xenstore_keys })
			with Api_errors.Server_error(code, params) ->
				Failure (Backend_error(code, params))

		let activate context ~task ~dp ~sr ~vdi =
			try
				let read_write = Mutex.execute vdi_read_write_m
					(fun () -> 
						if not (Hashtbl.mem vdi_read_write (sr, vdi)) then error "VDI.activate: doesn't know if sr:%s vdi:%s is RO or RW" sr vdi;
						Hashtbl.find vdi_read_write (sr, vdi)) in
				for_vdi ~task ~sr ~vdi "VDI.activate"
					(fun device_config _type sr self ->
						(* If the backend doesn't advertise the capability then do nothing *)
						if List.mem Smint.Vdi_activate (Sm.capabilities_of_driver _type)
						then Sm.vdi_activate device_config _type sr self read_write
						else info "%s sr:%s does not support vdi_activate: doing nothing" dp (Ref.string_of sr)
					);
				Success Unit
			with Api_errors.Server_error(code, params) ->
				Failure (Backend_error(code, params))

		let deactivate context ~task ~dp ~sr ~vdi =
			try
				for_vdi ~task ~sr ~vdi "VDI.deactivate"
					(fun device_config _type sr self ->
						(* If the backend doesn't advertise the capability then do nothing *)
						if List.mem Smint.Vdi_activate (Sm.capabilities_of_driver _type)
						then Sm.vdi_deactivate device_config _type sr self
						else info "%s sr:%s does not support vdi_activate: doing nothing" dp (Ref.string_of sr)
					);
				Success Unit
			with Api_errors.Server_error(code, params) ->
				Failure (Backend_error(code, params))

		let detach context ~task ~dp ~sr ~vdi =
			try
				for_vdi ~task ~sr ~vdi "VDI.detach"
					(fun device_config _type sr self ->
						Sm.vdi_detach device_config _type sr self
					);
				Mutex.execute vdi_read_write_m
					(fun () -> Hashtbl.remove vdi_read_write (sr, vdi));
				Success Unit
			with Api_errors.Server_error(code, params) ->
				Failure (Backend_error(code, params))

		let stat context ~task ?dp ~sr ~vdi () = assert false

		let require_uuid vdi_info = 
			match vdi_info.Smint.vdi_info_uuid with
				| Some uuid -> uuid
				| None -> failwith "SM backend failed to return <uuid> field" 

		let create context ~task ~sr ~name_label ~name_description ~virtual_size ~ty ~params =
			Server_helpers.exec_with_new_task "VDI.create" ~subtask_of:(Ref.of_string task)
				(fun __context ->

					let sr = Ref.of_string sr in
					let vi = 
						Sm.call_sm_functions ~__context ~sR:sr
							(fun device_config _type ->
								Sm.vdi_create device_config _type sr params ty 
									virtual_size name_label name_description
							) in
					(* The current backends stash data directly in the db *)
					let uuid = require_uuid vi in
					let ref = Db.VDI.get_by_uuid ~__context ~uuid in

					let actual_size = Db.VDI.get_virtual_size ~__context ~self:ref in
					Success (NewVdi { vdi = Ref.string_of ref; virtual_size = actual_size })
				)

		let destroy context ~task ~sr ~vdi =
			try
				for_vdi ~task ~sr ~vdi "VDI.destroy"
					(fun device_config _type sr self ->
						Sm.vdi_delete device_config _type sr self
					);
				Mutex.execute vdi_read_write_m
					(fun () -> Hashtbl.remove vdi_read_write (sr, vdi));
				Success Unit
			with Api_errors.Server_error(code, params) ->
				Failure (Backend_error(code, params))
	end
end

module type SERVER = sig
	val process : unit -> Rpc.call -> Rpc.response 
end

let make_local () =
	(module Server(Storage_impl.Wrapper(Builtin_impl)) : SERVER)

let make_remote host =
	(module Server(Storage_impl.Wrapper(Storage_proxy.Proxy(struct let rpc call = Rpc_client.do_rpc ~version:"1.0" ~host ~port:8080 ~path:"/" call end))) : SERVER)

let bind ~__context ~pbd = 
	(* Start the VM if necessary, discover its domid *)
	let driver = System_domains.storage_driver_domain_of_pbd ~__context ~pbd in
	if Db.VM.get_power_state ~__context ~self:driver = `Halted then begin
		info "PBD %s driver domain %s is offline: starting" (Ref.string_of pbd) (Ref.string_of driver);
		Helpers.call_api_functions ~__context 
			(fun rpc session_id -> XenAPI.VM.start rpc session_id driver false false);
	end;
	let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:driver) in
	let ip_of driver =
		(* Find the VIF on the Host internal management network *)
		let vifs = Db.VM.get_VIFs ~__context ~self:driver in
		let hin = Helpers.get_host_internal_management_network ~__context in
		let ip = 
			let vif =
				try
					List.find (fun vif -> Db.VIF.get_network ~__context ~self:vif = hin) vifs
				with Not_found -> failwith (Printf.sprintf "PBD %s driver domain %s has no VIF on host internal management network" (Ref.string_of pbd) (Ref.string_of driver)) in
			match Xapi_udhcpd.get_ip ~__context vif with
				| Some (a, b, c, d) -> Printf.sprintf "%d.%d.%d.%d" a b c d
				| None -> failwith (Printf.sprintf "PBD %s driver domain %s has no IP on the host internal management network" (Ref.string_of pbd) (Ref.string_of driver)) in

		info "PBD %s driver domain domid:%d ip:%s" (Ref.string_of pbd) domid ip;
		if not(System_domains.wait_for (System_domains.pingable ip))
		then failwith (Printf.sprintf "PBD %s driver domain %s is not responding to IP ping" (Ref.string_of pbd) (Ref.string_of driver));
		if not(System_domains.wait_for (System_domains.queryable ip 8080))
		then failwith (Printf.sprintf "PBD %s driver domain %s is not responding to XMLRPC query" (Ref.string_of pbd) (Ref.string_of driver));
		ip in

	let dom0 = Helpers.get_domain_zero ~__context in
	let module Impl = (val (if driver = dom0 then make_local () else make_remote (ip_of driver)): SERVER) in 
	let sr = Db.PBD.get_SR ~__context ~self:pbd in
	Storage_mux.register sr (Impl.process ()) domid

let unbind ~__context ~pbd =
	let sr = Db.PBD.get_SR ~__context ~self:pbd in
	Storage_mux.unregister sr

let rpc call = Storage_mux.Server.process () call

(** [datapath_of_vbd domid device] returns the name of the datapath which corresponds
    to device [device] on domain [domid] *)
let datapath_of_vbd ~domid ~device =
	Printf.sprintf "vbd/%d/%s" domid device

let unexpected_result expected x = match x with
	| Success _ ->
		failwith (Printf.sprintf "Run-time type error. Expected %s; got: %s" expected (string_of_result x))
	| Failure Sr_not_attached ->
		failwith "Storage_access failed with Sr_not_attached"
	| Failure (Backend_error(code, params)) ->
		raise (Api_errors.Server_error(code, params))
	| Failure (Internal_error x) ->
		failwith (Printf.sprintf "Storage_access failed with: %s" x)
	| Failure Illegal_transition(a, b) ->
		failwith (Printf.sprintf "Storage_access failed with %s" (string_of_result x))

let expect_vdi f x = match x with
	| Success (Vdi v) -> f v
	| _ -> unexpected_result "Vdi _" x

let expect_newvdi f x = match x with
	| Success (NewVdi v) -> f v
	| _ -> unexpected_result "NewVdi _" x

let expect_unit f x = match x with
	| Success Unit -> f ()
	| _ -> unexpected_result "()" x

let expect_string f x = match x with
	| Success (String x) -> f x
	| _ -> unexpected_result "String _" x

let pbd_of_sr ~__context ~sr =
	let pbd, _ = 
		try 
			List.find (fun (_, pbd_r) -> pbd_r.API.pBD_SR = sr) (Helpers.get_my_pbds __context) 
		with Not_found -> failwith (Printf.sprintf "pbd_of_sr failed to find PBD for SR %s" (Ref.string_of sr)) in
	pbd

(** [on_vdi __context vbd domid f] calls [f rpc dp sr vdi] which is
    useful for executing Storage_interface.Client.VDI functions  *)
let on_vdi ~__context ~vbd ~domid f =
	let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
	let sr = Db.VDI.get_SR ~__context ~self:vdi in
	let pbd = pbd_of_sr ~__context ~sr in
	bind ~__context ~pbd;
	let device = Db.VBD.get_device ~__context ~self:vbd in
	let task = Context.get_task_id __context in
	let dp = Client.DP.create rpc (Ref.string_of task) (datapath_of_vbd ~domid ~device) in
	f (Ref.string_of task) dp (Ref.string_of sr) (Ref.string_of vdi)

(** [attach_and_activate __context vbd domid f] calls [f xenstore_keys] where
    [xenstore_keys] is the result of attaching a VDI which is also activated.
    This should be used everywhere except the migrate code, where we want fine-grained
    control of the ordering of attach/activate/deactivate/detach *)
let attach_and_activate ~__context ~vbd ~domid f =
	let read_write = Db.VBD.get_mode ~__context ~self:vbd = `RW in
	on_vdi ~__context ~vbd ~domid
		(fun task dp sr vdi ->
			expect_vdi
				(fun blkback ->
					expect_unit
						(fun () ->
							f blkback
						) (Client.VDI.activate rpc task dp sr vdi)
				) (Client.VDI.attach rpc task dp sr vdi read_write)
		)

(** [deactivate_and_detach __context vbd domid] idempotent function which ensures
    that any attached or activated VDI gets properly deactivated and detached. *)
let deactivate_and_detach ~__context ~vbd ~domid =
	(* It suffices to destroy the datapath: any attached or activated VDIs will be
	   automatically detached and deactivated. *)
	on_vdi ~__context ~vbd ~domid
		(fun task dp sr vdi ->
			expect_unit (fun () -> ())
				(Client.DP.destroy rpc task dp false)
		)

let diagnostics ~__context =
	expect_string (fun x -> x) (Client.DP.diagnostics rpc ())

let dp_destroy ~__context dp allow_leak =
	let task = Context.get_task_id __context in
	expect_unit (fun () -> ())
		(Client.DP.destroy rpc (Ref.string_of task) dp allow_leak)

(* Set my PBD.currently_attached fields in the Pool database to match the local one *)
let resynchronise_pbds ~__context ~pbds =
	let task = Context.get_task_id __context in
	let srs = Client.SR.list rpc (Ref.string_of task) in
	debug "Currently-attached SRs according to the storage layer: [ %s ]" (String.concat "; " srs);
	List.iter
		(fun self ->
			let sr = Db.PBD.get_SR ~__context ~self in
			let value = List.mem (Ref.string_of sr) srs in
			let value = if value then begin
				(* Look for a failed driver domain *)
				let domain = System_domains.storage_driver_domain_of_pbd ~__context ~pbd:self in
				if Db.VM.get_power_state ~__context ~self:domain <> `Running then begin
					error "SR %s has PBD %s which is supposed to be attached but the driver domain %s is offline" (Ref.string_of sr) (Ref.string_of self) (Ref.string_of domain);
					false
				end else true
			end else value in

			debug "Setting PBD %s currently_attached <- %b" (Ref.string_of self) value;
			Db.PBD.set_currently_attached ~__context ~self ~value
		) pbds

(* -------------------------------------------------------------------------------- *)
(* The following functions are symptoms of a broken interface with the SM layer.
   They should be removed, by enhancing the SM layer. *)

open Vdi_automaton

(* This is a layering violation. The layers are:
     xapi: has a pool-wide view
     storage_impl: has a host-wide view of SRs and VDIs
     SM: has a SR-wide view
   Unfortunately the SM is storing some of its critical state (VDI-host locks) in the xapi
   metadata rather than on the backend storage. The xapi metadata is generally not authoritative
   and must be synchronised against the state of the world. Therefore we must synchronise the
   xapi view with the storage_impl view here. *)
let refresh_local_vdi_activations ~__context =
	let all_vdi_recs = Db.VDI.get_all_records ~__context in
	let host_key = Printf.sprintf "host_%s" (Ref.string_of (Helpers.get_localhost ~__context)) in

	(* If this VDI is currently locked to this host, remove the lock *)
	let unlock_vdi (vdi_ref, vdi_rec) = 
		(* VDI is already unlocked is the common case: avoid eggregious logspam *)
		if List.mem_assoc host_key vdi_rec.API.vDI_sm_config then begin
			info "Unlocking VDI %s" (Ref.string_of vdi_ref);
			try
				Db.VDI.remove_from_sm_config ~__context ~self:vdi_ref ~key:host_key
			with e ->
				error "Failed to unlock VDI %s: %s" (Ref.string_of vdi_ref) (ExnHelper.string_of_exn e)
		end in
	(* Lock this VDI to this host *)
	let lock_vdi (vdi_ref, vdi_rec) ro_rw = 
		info "Locking VDI %s" (Ref.string_of vdi_ref);
		if not(List.mem_assoc host_key vdi_rec.API.vDI_sm_config) then begin
			try
				Db.VDI.add_to_sm_config ~__context ~self:vdi_ref ~key:host_key ~value:(string_of_ro_rw ro_rw)
			with e ->
				error "Failed to lock VDI %s: %s" (Ref.string_of vdi_ref) (ExnHelper.string_of_exn e)
		end in
	let remember key ro_rw = 
		(* The module above contains a hashtable of R/O vs R/W-ness *)
		Mutex.execute Builtin_impl.VDI.vdi_read_write_m
			(fun () -> Hashtbl.replace Builtin_impl.VDI.vdi_read_write key (ro_rw = RW)) in

	let task = Ref.string_of (Context.get_task_id __context) in
	let srs = Client.SR.list rpc task in
	List.iter 
		(fun (vdi_ref, vdi_rec) ->
			try
			let sr = Ref.string_of vdi_rec.API.vDI_SR in
			let vdi = Ref.string_of vdi_ref in
			if List.mem sr srs
			then
				match Client.VDI.stat rpc ~task ~sr ~vdi () with
					| Success (State (Activated RO)) -> 
						lock_vdi (vdi_ref, vdi_rec) RO;
						remember (sr, vdi) RO
					| Success (State (Activated RW)) -> 
						lock_vdi (vdi_ref, vdi_rec) RW;
						remember (sr, vdi) RW
					| Success (State (Attached RO)) ->
						unlock_vdi (vdi_ref, vdi_rec);
						remember (sr, vdi) RO
					| Success (State (Attached RW)) ->
						unlock_vdi (vdi_ref, vdi_rec);
						remember (sr, vdi) RW
					| Success (State Detached) ->
						unlock_vdi (vdi_ref, vdi_rec)
					| Success (Vdi _ | NewVdi _ | Unit | String _)
					| Failure _ as r -> error "Unable to query state of VDI: %s, %s" vdi (string_of_result r)
			else unlock_vdi (vdi_ref, vdi_rec)
			with e -> error "Skipping VDI %s: %s" (Ref.string_of vdi_ref) (Printexc.to_string e)
		) all_vdi_recs

(* This is a symptom of the ordering-sensitivity of the SM backend: it is not possible
   to upgrade RO -> RW or downgrade RW -> RO on the fly.
   One possible fix is to always attach RW and enforce read/only-ness at the VBD-level.
   However we would need to fix the LVHD "attach provisioning mode". *)
let vbd_attach_order ~__context vbds = 
	(* return RW devices first since the storage layer can't upgrade a
	   'RO attach' into a 'RW attach' *)
	let rw, ro = List.partition (fun self -> Db.VBD.get_mode ~__context ~self = `RW) vbds in
	rw @ ro

let vbd_detach_order ~__context vbds = List.rev (vbd_attach_order ~__context vbds)

(* This is because the current backends want SR.attached <=> PBD.currently_attached=true.
   It would be better not to plug in the PBD, so that other API calls will be blocked. *)
let destroy_sr ~__context ~sr =
	let pbds = Db.SR.get_PBDs ~__context ~self:sr in
	let localhost = Helpers.get_localhost ~__context in

	let task = Ref.string_of (Context.get_task_id __context) in

	expect_unit (fun () -> ())
		(Client.SR.attach rpc task (Ref.string_of sr));	
	(* The current backends expect the PBD to be temporarily set to currently_attached = true *)
	List.iter
		(fun self ->
			if Db.PBD.get_host ~__context ~self = localhost
			then Db.PBD.set_currently_attached ~__context ~self ~value:true
		) pbds;
	expect_unit (fun () -> ())
		(Client.SR.destroy rpc task (Ref.string_of sr));	
	(* All PBDs are clearly currently_attached = false now *)
	List.iter (fun self -> Db.PBD.set_currently_attached ~__context ~self ~value:false) pbds

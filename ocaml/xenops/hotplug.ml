(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

open Printf
open Stringext

open Device_common

module D = Debug.Debugger(struct let name = "hotplug" end)
open D

(** Time we allow for the hotplug scripts to run before we assume something bad has
    happened and abort *)
let hotplug_timeout = 60. *. 20. (* seconds *)

(** If we can't execute the losetup program (for example) *)
exception External_command_failure of string
(** Timeout waiting for the backend hotplug scripts *)
exception Device_timeout of device
(** Timeout waiting for the frontend hotplug scripts (in the case of dom0) *)
exception Frontend_device_timeout of device
(** An explicit failure waiting for the frontend hotplug scripts (in the case of dom0) *)
exception Frontend_device_error of string
(** Ff we can't find the file we're trying to losetup *)
exception Loopdev_error of string
(** we can't delete a loop device *)
exception Loopdev_delete_error
(** If all the loopback devices are busy *)
exception Loopdev_all_busy

(** Interface to synchronise with minimal hotplug scripts *)

(* Notes about hotplug:
   We do all the heavy lifting (allocating loopback devices, checking for sharing
   problems) but we still need to be able to synchronise with the kernel (definitely
   for unplug, not sure about plug) *)

(* We store some transient data elsewhere in xenstore to avoid it getting
   deleted by accident when a domain shuts down. We should always zap this
   tree on boot. *)
let private_path = "/xapi"

(* The private data path is only used by xapi and ignored by frontend and backend *)
let get_private_path domid = sprintf "%s/%d" private_path domid

let get_private_data_path_of_device (x: device) = 
	sprintf "%s/private/%s/%d" (get_private_path x.frontend.domid) (string_of_kind x.backend.kind) x.backend.devid

(* Path in xenstore where we stuff our transient hotplug-related stuff *)
let get_hotplug_path (x: device) =
	sprintf "%s/hotplug/%s/%d" (get_private_path x.frontend.domid) (string_of_kind x.backend.kind) x.backend.devid

(* The path in xenstore written to by the hotplug scripts *)
let status_node (x: device) = get_hotplug_path x ^ "/hotplug"

(* The path in xenstore written to by the frontend hotplug scripts *)
let frontend_status_node (x: device) = 
	sprintf "%s/frontend/%s/%d/hotplug" (get_private_path x.frontend.domid) (string_of_kind x.frontend.kind) x.frontend.devid

(* The path in xenstore written to /by/ the hotplug scripts when the (network) device
   is properly online.
   See xs-xen.pq.hq:91e986b8e49f netback-wait-for-hotplug *)
let connected_node ~xs (x: device) = 
	sprintf "%s/backend/%s/%d/%d/hotplug-status" (xs.Xs.getdomainpath x.backend.domid) (string_of_kind x.backend.kind) x.frontend.domid x.frontend.devid

(* CA-15605: node written to by tapdisk to report an error (eg opening .vhd files). *)
let tapdisk_error_node ~xs (x: device) = 
  sprintf "%s/backend/%s/%d/%d/tapdisk-error" (xs.Xs.getdomainpath x.backend.domid) (string_of_kind x.backend.kind) x.frontend.domid x.frontend.devid

(* CA-39745: node written to by blkback to report an error (eg opening an empty CDROM drive) *)
let blkback_error_node ~xs (x: device) = 
  sprintf "%s/error/backend/vbd/%d/%d/error" (xs.Xs.getdomainpath x.backend.domid) x.backend.domid x.frontend.devid

(* Poll a device to see whether it is instantaneously "online" where "online" means
   "currently-attached" in the database. The event thread AND the startup code call
   this function to resynchronise the state of the world with the database. 
   If we're called for a VIF backend we rely solely on the dom0 backend hotplug scripts.
   If we're called for a VBD (blkback or blktap) then we first check to see if the 
   shutdown has been requested but not completed: if so we consider the device online and
   expect the shutdown-done event to come later. If no shutdown-request node exists
   (ie not an API-initiated hotunplug; this is start of day) then we check the state 
   of the backend hotplug scripts. *)
let device_is_online ~xs (x: device) = 
  let backend_hotplug () = try xs.Xs.read (status_node x) = "online" with Xb.Noent -> false
  and backend_shutdown () = try ignore(xs.Xs.read (backend_shutdown_done_path_of_device ~xs x)); true with Xb.Noent -> false 
  and backend_request () = try ignore(xs.Xs.read (backend_shutdown_request_path_of_device ~xs x)); true with Xb.Noent -> false in

  match x.backend.kind with
  | Pci -> assert false (* PCI backend doesn't create online node *)
  | Vif -> backend_hotplug ()
  | ( Vbd | Tap ) -> 
      if backend_request () 
      then not(backend_shutdown ())
      else backend_hotplug ()

(* Poll a device (vif) to see whether it has hotplug-status = connected *)
let device_is_connected ~xs (x: device) =
	try
		let path = connected_node ~xs x in
		xs.Xs.read path = "connected"
	with Xb.Noent -> false

let wait_for_plug ~xs (x: device) = 
  debug "Hotplug.wait_for_plug: %s" (string_of_device x);
  try
    Stats.time_this "udev backend add event" 
      (fun () ->
    ignore(Watch.wait_for ~xs ~timeout:hotplug_timeout (Watch.value_to_appear (status_node x)));
      );
    debug "Synchronised ok with hotplug script: %s" (string_of_device x)
  with Watch.Timeout _ ->
    raise (Device_timeout x)

let wait_for_unplug ~xs (x: device) = 
  debug "Hotplug.wait_for_unplug: %s" (string_of_device x);
  try
    Stats.time_this "udev backend remove event" 
      (fun () ->
    ignore(Watch.wait_for ~xs ~timeout:hotplug_timeout (Watch.key_to_disappear (status_node x)));
      );
    debug "Synchronised ok with hotplug script: %s" (string_of_device x)
  with Watch.Timeout _ ->
    raise (Device_timeout x)

(** Wait for the frontend device to become available to userspace *)
let wait_for_frontend_plug ~xs (x: device) =
  debug "Hotplug.wait_for_frontend_plug: %s" (string_of_device x);
  try
    let ok_watch = Watch.value_to_appear (frontend_status_node x) in
    let tapdisk_error_watch = Watch.value_to_appear (tapdisk_error_node ~xs x) in
	let blkback_error_watch = Watch.value_to_appear (blkback_error_node ~xs x) in
    Stats.time_this "udev frontend add event" 
      (fun () ->
	 match Watch.wait_for ~xs ~timeout:hotplug_timeout 
	 (Watch.any_of [ `OK, ok_watch; `Failed, tapdisk_error_watch; `Failed, blkback_error_watch ]) with
	 | `OK, _ ->
	     debug "Synchronised ok with frontend hotplug script: %s" (string_of_device x)
	 | `Failed, e ->
	     error "Failed waiting for frontend device %s: %s" (string_of_device x) e;
	     raise (Frontend_device_error e)
      );
  with Watch.Timeout _ ->
    error "Timed out waiting for the frontend udev event to fire on device: %s" (string_of_device x);
    raise (Frontend_device_timeout x)

let wait_for_frontend_unplug ~xs (x: device) =
  debug "Hotplug.wait_for_frontend_unplug: %s" (string_of_device x);
  try
    let path = frontend_status_node x in
    Stats.time_this "udev frontend remove event" 
      (fun () ->
    ignore(Watch.wait_for ~xs ~timeout:hotplug_timeout (Watch.key_to_disappear path));
      );
    debug "Synchronised ok with frontend hotplug script: %s" (string_of_device x)
  with Watch.Timeout _ ->
    raise (Frontend_device_timeout x)


(* Wait for the device to be released by the backend driver (via udev) and
   then deallocate any resources which are registered (in our private bit of
   xenstore) *)
let release ~xs (x: device) =
	debug "Hotplug.release: %s" (string_of_device x);
	wait_for_unplug ~xs x;
	let path = get_hotplug_path x in
	xs.Xs.rm path


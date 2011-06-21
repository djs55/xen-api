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
(*
 * Helpers for Xen-specific functionality (i.e. that which is not needed by
 * the fakeserver).
 *)

exception Device_has_no_VBD

(** Returns the backend type ('vbd' or 'tap') used by this VDI *)
let kind_of_vdi ~__context ~self =
	let sr = Db.VDI.get_SR ~__context ~self in
	let physty = Device.Vbd.physty_of_string (Sm.sr_content_type ~__context ~sr) in
	Device.Vbd.kind_of_physty physty

(** Given a VBD, return a xenops device. Always called on the host where the VM is present *)
let device_of_vbd ~__context ~self = 
	let backend_domid = 
		if Db.VBD.get_empty ~__context ~self
		then 0
		else
			let vdi = Db.VBD.get_VDI ~__context ~self in
			let sr = Db.VDI.get_SR ~__context ~self:vdi in
			let pbd = Storage_access.pbd_of_sr ~__context ~sr in
			let driver_domain = Storage_access.driver_domain_of_pbd ~__context ~pbd in
			Int64.to_int (Db.VM.get_domid ~__context ~self:driver_domain) in
  let vm = Db.VBD.get_VM ~__context ~self in
  let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
  let hvm = Xc.with_intf (fun xc -> (Xc.domain_getinfo xc domid).Xc.hvm_guest) in
  let devid = Device_number.to_xenstore_key (Device_number.of_string hvm (Db.VBD.get_device ~__context ~self)) in
  let backend = { Device_common.domid = backend_domid; 
		  kind = Device_common.Vbd;
		  devid = devid } in
  Device_common.device_of_backend backend domid 

(** Given a VIF, return a xenops device *)
let device_of_vif ~__context ~self = 
  let vm = Db.VIF.get_VM ~__context ~self in
  let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
  let devid = Helpers.devid_of_vif ~__context ~self in
  let backend = { Device_common.domid = 0; 
		  kind = Device_common.Vif;
		  devid = devid } in
  Device_common.device_of_backend backend domid 

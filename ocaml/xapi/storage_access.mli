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
(**
 * @group Storage
 *)
 
module SR :
sig
  val attach : __context:Context.t -> self:API.ref_SR -> unit
  val detach : __context:Context.t -> self:API.ref_SR -> unit
end

module VDI_lowlevel :
sig
  val initialise_refcounts_from_db : unit -> unit

  val attach     : __context:Context.t -> self:API.ref_VDI -> mode:[`RO|`RW] -> unit
  val detach     : __context:Context.t -> self:API.ref_VDI -> unit
  val activate   : __context:Context.t -> self:API.ref_VDI -> mode:[`RO|`RW] -> string
  val deactivate : __context:Context.t -> self:API.ref_VDI -> unit

  val check_enclosing_sr_for_capability : Context.t -> Smint.capability -> [ `VDI ] Ref.t -> bool
end

module VDI : sig
  val attach     : __context:Context.t -> self:API.ref_VDI -> mode:[`RO|`RW] -> unit
  val detach     : __context:Context.t -> self:API.ref_VDI -> unit
  val activate   : __context:Context.t -> self:API.ref_VDI -> mode:[`RO|`RW] -> string
  val deactivate : __context:Context.t -> self:API.ref_VDI -> unit
  val get_physical_path : self:API.ref_VDI -> string
  val get_physical_device : self:API.ref_VDI -> string
end

val use_vdi                          : __context:Context.t -> vdi:API.ref_VDI -> mode:[`RO|`RW] -> string
val deactivate_and_detach            : __context:Context.t -> vdi:API.ref_VDI -> unit
val with_careful_attach_and_activate : __context:Context.t -> vdis:(API.ref_VDI * [`RO|`RW]) list -> leave_activated:bool -> (unit -> 'a ) -> 'a
val use_vdi_from_vbd                 : __context:Context.t -> [`VBD] Ref.t -> string


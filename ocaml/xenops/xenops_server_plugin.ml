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

open Xenops_interface

module type S = sig
	module VM : sig
		val create: Vm.t -> unit
		val build: Vm.t -> Vbd.t list -> Vif.t list -> unit
		val create_device_model: Vm.t -> unit
		val destroy: Vm.t -> unit
		val pause: Vm.t -> unit
		val unpause: Vm.t -> unit

		val suspend: Vm.t -> disk -> unit
		val resume: Vm.t -> disk -> unit

		val get_state: Vm.t -> Vm.state
	end
	module VBD : sig
		val plug: Vm.id -> Vbd.t -> unit
		val unplug: Vm.id -> Vbd.t -> unit

		val get_state: Vm.id -> Vbd.t -> Vbd.state
	end
	module VIF : sig
		val plug: Vm.id -> Vif.t -> unit
		val unplug: Vm.id -> Vif.t -> unit

		val get_state: Vm.id -> Vif.t -> Vif.state
	end
end

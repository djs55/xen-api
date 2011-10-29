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
		val create: Vm.t -> unit option * error option
		val build: Vm.t -> Vbd.t list -> Vif.t list -> unit option * error option
		val destroy: Vm.t -> unit option * error option
		val pause: Vm.t -> unit option * error option
		val unpause: Vm.t -> unit option * error option

		val suspend: Vm.t -> disk -> unit option * error option
		val resume: Vm.t -> disk -> unit option * error option

		val get_power_state: Vm.t -> power_state option * error option
	end
	module VBD : sig
		val plug: Vm.id -> Vbd.t -> unit option * error option
		val unplug: Vm.id -> Vbd.t -> unit option * error option

		val get_currently_attached: Vm.id -> Vbd.t -> bool option * error option
	end
	module VIF : sig
		val plug: Vm.id -> Vif.t -> unit option * error option
		val unplug: Vm.id -> Vif.t -> unit option * error option

		val get_currently_attached: Vm.id -> Vif.t -> bool option * error option
	end
end

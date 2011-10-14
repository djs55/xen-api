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
		val make: Vm.t -> unit option * error option
		val build: Vm.t -> unit option * error option
		val destroy: Vm.t -> unit option * error option
		val pause: Vm.t -> unit option * error option
		val unpause: Vm.t -> unit option * error option
	end
end

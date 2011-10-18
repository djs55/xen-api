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
open Xenops_utils

module VM = struct
	let make vm = throw Unimplemented
	let build vm = throw Unimplemented
	let destroy vm = throw Unimplemented
	let pause vm = throw Unimplemented
	let unpause vm = throw Unimplemented

	let get_power_state vm = throw Unimplemented
end

module VBD = struct
	let plug vm vbd = throw Unimplemented
	let unplug vm vbd = throw Unimplemented

	let get_currently_attached vm vbd = throw Unimplemented
end

module VIF = struct
	let plug vm vbd = throw Unimplemented
	let unplug vm vbd = throw Unimplemented

	let get_currently_attached vm vbd = throw Unimplemented
end

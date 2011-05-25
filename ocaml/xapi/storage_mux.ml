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

module type Processor = sig
	val process: unit -> Rpc.call -> Rpc.response
end
(*
module Remote=Server(Storage_impl.Wrapper(Storage_proxy.Proxy(struct let rpc call = Rpc_client.do_rpc ~version:"1.0" ~host:"localhost" ~port:8080 ~path:"/" call end)))
*)
open Storage_interface

let plugins : (API.ref_SR, (module Processor)) Hashtbl.t = Hashtbl.create 10

let register sr m = Hashtbl.add plugins sr m

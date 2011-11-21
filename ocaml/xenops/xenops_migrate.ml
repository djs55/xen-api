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

(* Describes what we need on the remote end for this migration to work *)
type prereqs = {
	x: string;
}

let receive req s _ =
	debug "receiving VM";
	Http_svr.headers s (Http.http_200_ok ())

let transmit vm_t url =
	let open Xmlrpc_client in
	debug "transmit %s to %s" vm_t.Vm.name url;
	let url = Http.Url.of_string url in
	let transport = transport_of_url url in
	let req = Http.Request.make ~version:"1.0" ?auth:(Http.Url.auth_of url) ~user_agent:"xenopsd" Http.Post (Http.Url.uri_of url) in
	with_transport transport
		(fun fd ->
			Http_client.rpc fd req
				(fun response _ ->
					debug "Received %s" response.Http.Response.code
				)
		)

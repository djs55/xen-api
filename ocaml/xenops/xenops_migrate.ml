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

let ( |> ) a b = b a

(* Describes what we need on the remote end for this migration to work *)
type prereqs = {
	x: string;
}

module Receiver = struct
	type state = unit
	let initial = ()
	let next state call = Rpc.success Rpc.Null, state
end

type sender_state = unit

let rec receiver_loop req s state =
	let next_req, next_state = try
		let call = Unixext.really_read_string s (req.Http.Request.content_length |> Opt.unbox |> Int64.to_int) |> Jsonrpc.call_of_string in
		let response, next_state = Receiver.next state call in
		let body = response |> Jsonrpc.string_of_response in
		let length = body |> String.length |> Int64.of_int in
		let response = Http.Response.make ~version:"1.1" ~length ~body "200" "OK" in
		response |> Http.Response.to_wire_string |> Unixext.really_write_string s;
		(* We need to unmarshal the next HTTP request ourselves. *)
		match Http_svr.request_of_bio (Buf_io.of_fd s) with
			| None ->
				debug "Failed to parse HTTP request";
				failwith "Failed to parse HTTP request"
			| Some req -> req, next_state
	with e ->
		debug "Receiver thread caught: %s" (Printexc.to_string e);
		raise e in
	receiver_loop next_req s next_state

let receive req s _ =
	let _, _ = receiver_loop req s Receiver.initial in
	()

let rpc url rpc fd =
	let body = rpc |> Jsonrpc.string_of_call in
	let length = body |> String.length |> Int64.of_int in
	let req = Http.Request.make ~version:"1.1" ?auth:(Http.Url.auth_of url) ~user_agent:"xenopsd" ~length ~body Http.Post (Http.Url.uri_of url) in
	Http_client.rpc fd req
		(fun response _ ->
			Unixext.really_read_string fd (response.Http.Response.content_length |> Opt.unbox |> Int64.to_int)
		)

let transmit vm_t url =
	let open Xmlrpc_client in
	debug "transmit %s to %s" vm_t.Vm.name url;
	let url = Http.Url.of_string url in
	let transport = transport_of_url url in
	with_transport transport
		(fun fd ->
			let _ = rpc url (Rpc.call "hello" []) fd in
			let _ = rpc url (Rpc.call "there" []) fd in
			()
		)

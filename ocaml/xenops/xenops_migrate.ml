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

let local_rpc call =
	let open Xmlrpc_client in
	XMLRPC_protocol.rpc ~transport:(Unix "/var/xapi/xenopsd") ~http:(xmlrpc ~version:"1.0" "/") call

let _metadata = "VM.import_metadata"

module Receiver = struct
	type created_object =
		| Vm_metadata of Vm.id
		| Vm_created of Vm.id

	type state =
		| Waiting_metadata
		| Received_metadata of Vm.id
		| Error of string
	with rpc

	type t = state * created_object list

	let cleanup = function
		| Vm_metadata id ->
			debug "Removing VM metadata for VM id %s" id;
			Client.VM.remove local_rpc id |> unwrap
		| Vm_created id ->
			debug "Destroying VM id %s" id;
			Client.VM.destroy local_rpc id |> unwrap

	let initial = Waiting_metadata, []
	let next (state, created_objects) call = match state, call.Rpc.name, call.Rpc.params with
		| Waiting_metadata, _metadata, [ Rpc.String md ] ->
			let vm = md |> Client.VM.import_metadata local_rpc |> unwrap in
			let created_objects = Vm_metadata vm :: created_objects in
			Client.VM.create local_rpc vm |> unwrap;
			let created_objects = Vm_created vm :: created_objects in
			Received_metadata vm, created_objects
		| state, name, _ ->
			List.iter cleanup created_objects;
			Error (Printf.sprintf "Unexpected call. State = %s; Call = %s" (state |> rpc_of_state |> Jsonrpc.to_string) name), []

	let string_of_state x = x |> fst |> rpc_of_state |> Jsonrpc.to_string
end

type sender_state = unit

let rec receiver_loop req s state =
	let next_req, next_state = try
		let call = Unixext.really_read_string s (req.Http.Request.content_length |> Opt.unbox |> Int64.to_int) |> Jsonrpc.call_of_string in
		let next_state = Receiver.next state call in
		debug "previous state = %s; next state = %s" (Receiver.string_of_state state) (Receiver.string_of_state next_state);
		let response = Rpc.success (next_state |> fst |> Receiver.rpc_of_state) in
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
			let metadata = Client.VM.export_metadata local_rpc vm_t.Vm.id |> unwrap in
			let _ = rpc url (Rpc.call _metadata [ Rpc.String metadata ]) fd in
			let _ = rpc url (Rpc.call "hello" []) fd in
			let _ = rpc url (Rpc.call "there" []) fd in
			()
		)

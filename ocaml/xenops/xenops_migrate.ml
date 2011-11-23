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
open Xenops_client

let ( |> ) a b = b a

let local_rpc call =
	let open Xmlrpc_client in
	XMLRPC_protocol.rpc ~transport:(Unix "/var/xapi/xenopsd") ~http:(xmlrpc ~version:"1.0" "/") call

let _metadata = "VM.import_metadata"
let _failure = "VM.client_migrate_failed"

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
			Client.VM.destroy local_rpc id |> success |> wait_for_task local_rpc |> success_task local_rpc |> ignore_task

	let initial = Waiting_metadata, []
	let next (state, created_objects) call = match state, call.Rpc.name, call.Rpc.params with
		| Waiting_metadata, call, [ Rpc.String md ] when call = _metadata ->
			let vm = md |> Client.VM.import_metadata local_rpc |> unwrap in
			let created_objects = Vm_metadata vm :: created_objects in
			Client.VM.create local_rpc vm |> success |> wait_for_task local_rpc |> success_task local_rpc |> ignore_task;
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

let receive_memory req s _ =
	debug "XXX receive memory";
	let response = Http.Response.make ~version:"1.1" "200" "OK" in
	response |> Http.Response.to_wire_string |> Unixext.really_write_string s


let http_post url length body =
	Http.Request.make ~version:"1.1" ?auth:(Http.Url.auth_of url) ~user_agent:"xenopsd" ~length ~body Http.Post (Http.Url.uri_of url)

let http_put url =
	Http.Request.make ~version:"1.1" ~keep_alive:false ?auth:(Http.Url.auth_of url) ~user_agent:"xenopsd" Http.Put (Http.Url.uri_of url)

let remote_rpc url rpc fd =
	let body = rpc |> Jsonrpc.string_of_call in
	let length = body |> String.length |> Int64.of_int in
	Http_client.rpc fd (http_post url length body)
		(fun response _ ->
			let body = Unixext.really_read_string fd (response.Http.Response.content_length |> Opt.unbox |> Int64.to_int) in
		debug "body = [%s]" body;
		body |> Jsonrpc.response_of_string |> (fun x -> x.Rpc.contents) |> Receiver.state_of_rpc
		)

let send_metadata url metadata fd =
	let open Receiver in
	match remote_rpc url (Rpc.call _metadata [ Rpc.String metadata ]) fd with
		| Received_metadata id -> id
		| x -> failwith (Printf.sprintf "Unexpected response: %s" (x |> rpc_of_state |> Jsonrpc.to_string))

let send_failure url fd =
	let _ = remote_rpc url (Rpc.call _failure []) fd in
	()

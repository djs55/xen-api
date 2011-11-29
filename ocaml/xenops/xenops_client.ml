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
open Xmlrpc_client
let default_path = "/var/xapi/xenopsd"
let forwarded_path = default_path ^ ".forwarded"
let transport = ref (Unix default_path)

let ( |> ) a b = b a

let rpc call =
	XMLRPC_protocol.rpc ~transport:!transport
		~http:(xmlrpc ~version:"1.0" "/") call

let success = function
	| (_, Some x) -> failwith (Jsonrpc.to_string (rpc_of_error x))
	| (Some x, _) -> x
	| None, None -> failwith "protocol error"

let query url =
	let transport = transport_of_url url in
	let rpc call =
		XMLRPC_protocol.rpc ~transport ~http:(xmlrpc ~version:"1.0" (Http.Url.uri_of url)) call in
	Client.query rpc () |> success

let event_wait rpc p =
	let finished = ref false in
	let event_id = ref None in
	while not !finished do
		let deltas, next_id = Client.UPDATES.get rpc !event_id (Some 30) |> success in
		event_id := next_id;
		List.iter (fun d -> if p d then finished := true) deltas;
	done

let wait_for_task rpc id =
	Printf.fprintf stderr "wait_for id = %s\n%!" id;
	let finished = function
		| Dynamic.Task_t t ->
			Printf.fprintf stderr "got event for id %s\n%!" id;
			if t.Task.id = id then begin
				match t.Task.result with
				| Task.Pending _ -> false
				| Task.Completed _ -> true
				| Task.Failed _ -> true
			end else false
		| x ->
			Printf.fprintf stderr "ignore event on %s\n%!" (x |> Dynamic.rpc_of_t |> Jsonrpc.to_string);
			false in 
	event_wait rpc finished;
	id

let success_task rpc id =
	let t = Client.TASK.stat rpc id |> success in
	match t.Task.result with
	| Task.Completed _ -> t
	| Task.Failed x -> failwith (Jsonrpc.to_string (rpc_of_error x))
	| Task.Pending _ -> failwith "task pending"

let ignore_task (t: Task.t) = ()


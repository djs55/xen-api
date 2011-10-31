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
let name = "xenopsd"
let default_pidfile = Printf.sprintf "/var/run/%s.pid" name 
let log_file_path = Printf.sprintf "file:/var/log/%s.log" name 

module D = Debug.Debugger(struct let name = name end)
open D

open Pervasiveext 

let socket = ref None
let server = Http_svr.Server.empty ()

let path = "/var/xapi/xenopsd"

module Server = Xenops_interface.Server(Xenops_server)

let xmlrpc_handler process req bio =
    let body = Http_svr.read_body req bio in
    let s = Buf_io.fd_of bio in
    let rpc = Xmlrpc.call_of_string body in
	(* Xenops_utils.debug "Request: %s %s" rpc.Rpc.name (Jsonrpc.to_string (List.hd rpc.Rpc.params)); *)
	try
		let result = process (Xenops_server.make_context ()) rpc in
		(* Xenops_utils.debug "Response: success:%b %s" result.Rpc.success (Jsonrpc.to_string result.Rpc.contents); *)
		let str = Xmlrpc.string_of_response result in
		Http_svr.response_str req s str
	with Xenops_utils.Exception e ->
		let rpc = Xenops_interface.rpc_of_error_response (None, Some e) in
		Xenops_utils.debug "Caught %s" (Jsonrpc.to_string rpc);
		let str = Xmlrpc.string_of_response { Rpc.success = true; contents = rpc } in
		Http_svr.response_str req s str
	| e ->
		Xenops_utils.debug "Caught %s" (Printexc.to_string e);
		Http_svr.response_unauthorised ~req (Printf.sprintf "Go away: %s" (Printexc.to_string e)) s

let start path process =
    Unixext.mkdir_safe (Filename.dirname path) 0o700;
    Unixext.unlink_safe path;
    let domain_sock = Http_svr.bind (Unix.ADDR_UNIX(path)) "unix_rpc" in
    Http_svr.Server.add_handler server Http.Post "/" (Http_svr.BufIO (xmlrpc_handler process));
    Http_svr.start server domain_sock;
	socket := Some domain_sock



(* val reopen_logs: unit -> unit *)
let reopen_logs _ = 
  debug "Reopening logfiles";
  Logs.reopen ();
  debug "Logfiles reopened";
  []

let _ = 
  let pidfile = ref default_pidfile in
  let daemonize = ref false in
  let simulate = ref false in
  let clean = ref false in

  Arg.parse (Arg.align [
	       "-daemon", Arg.Set daemonize, "Create a daemon";
	       "-pidfile", Arg.Set_string pidfile, Printf.sprintf "Set the pid file (default \"%s\")" !pidfile;
		   "-simulate", Arg.Set simulate, "Use the simulator backend (default is the xen backend)";
		   "-clean", Arg.Set clean, "Remove any existing persistent configuration (useful for testing)";
	     ])
    (fun _ -> failwith "Invalid argument")
    (Printf.sprintf "Usage: %s [-daemon] [-pidfile filename]" name);

  Logs.reset_all [ log_file_path ];

  if !daemonize then Unixext.daemonize () else Xenops_utils.print_debug := true;

  Unixext.mkdir_rec (Filename.dirname !pidfile) 0o755;
  Unixext.pidfile_write !pidfile;

  if !clean then Xenops_utils.empty_database ();

  Xenops_server.set_backend
	  (Some (if !simulate
	  then (module Xenops_server_simulator: Xenops_server_plugin.S)
	  else (module Xenops_server_xen: Xenops_server_plugin.S)));

  start path Server.process;
  while true do
	  Thread.delay 60.
  done



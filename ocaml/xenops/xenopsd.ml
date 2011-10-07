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

let xmlrpc_handler process req bio =
    (*let body = Http_svr.read_body req bio in*)
    let s = Buf_io.fd_of bio in
    (*let rpc = Xmlrpc.call_of_string body in*)
    (* Printf.fprintf stderr "Request: %s %s\n%!" rpc.Rpc.name (Rpc.to_string (List.hd rpc.Rpc.params)); *)
	Http_svr.response_unauthorised ~req "Go away" s

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
 
  Arg.parse (Arg.align [
	       "-daemon", Arg.Set daemonize, "Create a daemon";
	       "-pidfile", Arg.Set_string pidfile, Printf.sprintf "Set the pid file (default \"%s\")" !pidfile;
	     ])
    (fun _ -> failwith "Invalid argument")
    (Printf.sprintf "Usage: %s [-daemon] [-pidfile filename]" name);

  Logs.reset_all [ log_file_path ];

  if !daemonize then Unixext.daemonize ();

  Unixext.mkdir_rec (Filename.dirname !pidfile) 0o755;
  Unixext.pidfile_write !pidfile;

  start path xmlrpc_handler;
  while true do
	  Thread.delay 60.
  done



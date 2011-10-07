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

  exit 0


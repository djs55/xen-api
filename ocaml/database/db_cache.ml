(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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


open Db_interface
open Threadext

module D=Debug.Make(struct let name="db_cache" end)
open D

let path = ref "/tmp/foo"

let cached_fd = ref None
let get_connection =
        fun () ->
                match !cached_fd with
                | Some fd -> fd
                | None ->
                        let x = Unixext.open_connection_unix_fd !path in
                        cached_fd := Some x;
                        x

let rpc url content_type request_txt =
        let version = "1.1" in
        let content_length = String.length request_txt in
        let request = Http.Request.make ~version
                ~user_agent:"database_test"
                ~body:request_txt
                ~length:(Int64.of_int content_length) Http.Post url in
        let open Xmlrpc_client in
        let fd = get_connection () in
        try
                Some (with_http request
                        (fun (response, fd) ->
                                match response.Http.Response.content_length with
                                        | None -> failwith "Need a content-length"
                                        | Some l -> Db_interface.String
                                                   (Unixext.really_read_string fd (Int64.to_int l))
                        ) fd)
        with e ->
                cached_fd := None;
                Unix.close fd;
                None

let m = Mutex.create ()
let rec rpc_common url content_type request_txt =
        match (Mutex.execute m (fun () -> rpc url content_type request_txt)) with
        | None -> rpc_common url content_type request_txt
        | Some x -> x

module Irmin_db = Db_rpc_client_v1.Make(struct
        let initialise () = ()
        let rpc request = rpc_common "/post_remote_db_access" "text/xml" request
end)

(** Masters will use this to modify the in-memory cache directly *)
module Memory_db : DB_ACCESS = Db_cache_impl

(** Slaves will use this to call the master by XMLRPC *)
module Remote_db : DB_ACCESS = Db_rpc_client_v1.Make(struct
	let initialise () = 
		ignore (Master_connection.start_master_connection_watchdog());
		ignore (Master_connection.open_secure_connection())
	let rpc request = Master_connection.execute_remote_fn request Constants.remote_db_access_uri
end)

module Local_db = Irmin_db

let get = function
	| Db_ref.In_memory _ -> (module Local_db  : DB_ACCESS)
	| Db_ref.Remote      -> (module Remote_db : DB_ACCESS)

let apply_delta_to_cache entry db_ref =
	let module DB = (Local_db : DB_ACCESS) in
    match entry with 
		| Redo_log.CreateRow(tblname, objref, kvs) ->
			debug "Redoing create_row %s (%s)" tblname objref;
			DB.create_row db_ref tblname kvs objref
		| Redo_log.DeleteRow(tblname, objref) ->
			debug "Redoing delete_row %s (%s)" tblname objref;
			DB.delete_row db_ref tblname objref
		| Redo_log.WriteField(tblname, objref, fldname, newval) ->
			debug "Redoing write_field %s (%s) [%s -> %s]" tblname objref fldname newval;
			DB.write_field db_ref tblname objref fldname newval

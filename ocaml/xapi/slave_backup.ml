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
(**
 * @group Pool Management
 *)

(** Immediately fetch a database backup from the master. If a flush_spec is given, with a list of db connections,
    then the backup is flushed to those connections; if no flush spec is given then the backup is flushed to all
    db conections.
*)

(* Read connections and gen counts and find if any are behind. If they are behind then figure out if we're allowed to
   write to them. Return list of connections that satisfy both these properties *)
let determine_backup_connections generation_count =
  let dbconns_and_gen_counts = Db_connections.get_dbs_and_gen_counts() in
  (* throw out dbconns that are up-to-date *)
  let dbconns_and_gen_counts = List.filter (fun (gen,_) -> gen<>generation_count) dbconns_and_gen_counts in
  List.map snd dbconns_and_gen_counts

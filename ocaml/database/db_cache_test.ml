(*
 * Copyright (C) 2010-2014 Citrix Systems Inc.
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

open Pervasiveext
open Db_cache_types

open OUnit

let create_test_db () =
  let schema = Test_schemas.many_to_many in
  let db = 
    ((fun x -> x)
     ++ (Db_backend.blow_away_non_persistent_fields schema)
     ++ (Db_upgrade.generic_database_upgrade))
      (Database.make schema) in
  db

let make_row time = List.fold_left (fun acc (k, v) -> Row.add time k v acc) Row.empty

let find tblname objref db =
  Table.find "VBD:1" (snd (TableSet.find "VBD" (Database.tableset db)))

let test_field_generation_count () =
  let db = create_test_db () in
  (* check that updating a field increases the modified time *)
  let db =
      Database.make Test_schemas.schema
   |> Db_upgrade.generic_database_upgrade
   |> Db_backend.blow_away_non_persistent_fields Test_schemas.schema
   |> add_row "VBD" "VBD:1" (make_row Generation.initial [ Db_names.ref, Schema.Value.String "VBD:1"; Db_names.uuid, Schema.Value.String "uuid"; "VM", Schema.Value.String "theVM" ])
   |> Database.branch in

  let stats, vbd_1 = find "VBD" "VBD:1" db in
  let db' = set_field "VBD" "VBD:1" "uuid" (Schema.Value.String "another uuid") db in
  let stats', vbd_1' = find "VBD" "VBD:1" db' in
  assert_equal ~printer:Int64.to_string stats.Stat.created stats'.Stat.created;
  assert_equal ~printer:Int64.to_string stats.Stat.deleted stats'.Stat.deleted;
  assert_equal ~printer:Int64.to_string (Int64.add stats.Stat.modified 1L) stats'.Stat.modified;
  (* check the generation count on the table has been bumped *)
  let stats, _ = TableSet.find "VBD" (Database.tableset db) in
  let stats', _ = TableSet.find "VBD" (Database.tableset db') in
  assert_equal ~printer:Int64.to_string stats.Stat.created stats'.Stat.created;
  assert_equal ~printer:Int64.to_string stats.Stat.deleted stats'.Stat.deleted;
  assert_equal ~printer:Int64.to_string (Int64.add stats.Stat.modified 1L) stats'.Stat.modified;
  (* check the generation count on the table is bumped again *)
  let db'' = add_row "VBD" "VBD:2" (make_row 0L [ Db_names.ref, Schema.Value.String "VBD:2"; Db_names.uuid, Schema.Value.String "uuid2"; "VM", Schema.Value.String "theVM2" ]) db' in
  let stats'', _ = TableSet.find "VBD" (Database.tableset db'') in
  assert_equal ~printer:Int64.to_string stats'.Stat.created stats''.Stat.created;
  assert_equal ~printer:Int64.to_string stats'.Stat.deleted stats''.Stat.deleted;
  assert_equal ~printer:Int64.to_string (Int64.add stats'.Stat.modified 1L) stats''.Stat.modified;

  let printer x = String.concat "; " (List.map (fun x -> Sexplib.Sexp.to_string_hum (Update.sexp_of_t x)) x) in
  let cmp a b =
          (* Every row should be involved at most once in our 'expected' lists
           * below so we can sort the lists into any order for comparison
           *)
        let a' = List.sort compare a and b' = List.sort compare b in
        List.fold_left (&&) true (List.map (fun (a, b) -> Update.equal a b) (List.combine a' b')) in

  (* We first did a writefield *)
  let the_write = Update.WriteField("VBD", "VBD:1", "uuid", Schema.Value.String "uuid", Schema.Value.String "another uuid") in
  let updates = Database.diff db db' in
  assert_equal ~printer ~cmp [ the_write ] updates;
  (* And then created another *)
  Printf.fprintf stderr "\nmergepoint = %s\n" (Sexplib.Sexp.to_string_hum (Database.sexp_of_t db'));
  Printf.fprintf stderr "\ntopic = %s\n%!" (Sexplib.Sexp.to_string_hum (Database.sexp_of_t db''));
  let values = List.fold_left (fun map (k, v) -> StringMap.add k v map) StringMap.empty
    [ "uuid", Schema.Value.String "uuid2"; "_ref", Schema.Value.String "VBD:2"; "VM", Schema.Value.String "theVM2" ] in
  let the_create = Update.Create("VBD", "VBD:2", values) in
  let updates = Database.diff db' db'' in
  assert_equal ~printer ~cmp [ the_create ] updates;
  (* Check we see them both: *)
  let updates = Database.diff db db'' in
  assert_equal ~printer ~cmp [ the_write; the_create ] updates;

  ()

let check_many_to_many () = 
  let db = create_test_db () in
  (* make a foo with bars = [] *)
  (* make a bar with foos = [] *)
  (* add 'bar' to foo.bars *)
  let db = 
    ((fun x -> x)
     ++ (set_field "foo" "foo:1" "bars" (add_to_set "bar:1" (Schema.Value.Set [])))
     ++ (add_row "foo" "foo:1" (Row.add 0L Db_names.ref (Schema.Value.String "foo:1") (Row.add 0L "bars" (Schema.Value.Set []) Row.empty)))
     ++ (add_row "bar" "bar:1" (Row.add 0L Db_names.ref (Schema.Value.String "bar:1") (Row.add 0L "foos" (Schema.Value.Set []) Row.empty)))) db in
  (* check that 'bar.foos' includes 'foo' *)
  let _, bar_1 = Table.find "bar:1" (snd (TableSet.find "bar" (Database.tableset db))) in
  let _, bar_foos = Row.find "foos" bar_1 in
  if bar_foos <> (Schema.Value.Set [ "foo:1" ])
  then failwith (Printf.sprintf "check_many_to_many: bar(bar:1).foos expected ('foo:1') got %s" (Sexplib.Sexp.to_string (Schema.Value.sexp_of_t bar_foos)));

  (* set foo.bars to [] *)
  (*		let foo_1 = Table.find "foo:1" (TableSet.find "foo" (Database.tableset db)) in*)
  let db = set_field "foo" "foo:1" "bars" (Schema.Value.Set []) db in
  (* check that 'bar.foos' is empty *)
  let _, bar_1 = Table.find "bar:1" (snd (TableSet.find "bar" (Database.tableset db))) in
  let _, bar_foos = Row.find "foos" bar_1 in
  if bar_foos <> (Schema.Value.Set [])
  then failwith (Printf.sprintf "check_many_to_many: bar(bar:1).foos expected () got %s" (Sexplib.Sexp.to_string (Schema.Value.sexp_of_t bar_foos)));
  (* add 'bar' to foo.bars *)
  let db = set_field "foo" "foo:1" "bars" (Schema.Value.Set [ "bar:1" ]) db in
  (* check that 'bar.foos' includes 'foo' *)
  let _, bar_1 = Table.find "bar:1" (snd (TableSet.find "bar" (Database.tableset db))) in
  let _, bar_foos = Row.find "foos" bar_1 in
  if bar_foos <> (Schema.Value.Set [ "foo:1" ])
  then failwith (Printf.sprintf "check_many_to_many: bar(bar:1).foos expected ('foo:1') got %s - 2" (Sexplib.Sexp.to_string (Schema.Value.sexp_of_t bar_foos)));
  (* delete 'bar' *)
  let db = remove_row "bar" "bar:1" db in
  (* check that 'foo.bars' is empty *)
  let _, foo_1 = Table.find "foo:1" (snd (TableSet.find "foo" (Database.tableset db))) in
  let _, foo_bars = Row.find "bars" foo_1 in
  if foo_bars <> (Schema.Value.Set [])
  then failwith (Printf.sprintf "check_many_to_many: foo(foo:1).foos expected () got %s" (Sexplib.Sexp.to_string (Schema.Value.sexp_of_t foo_bars)));
  ()


let _ =
  let suite = "db_cache" >:::
              [
                "field generation counts increase" >:: test_field_generation_count;
                "many to many" >:: check_many_to_many;
              ] in
  OUnit2.run_test_tt_main (OUnit.ounit2_of_ounit1 suite)

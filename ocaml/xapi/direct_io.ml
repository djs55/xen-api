(*
 * Copyright (C) 2011-2013 Citrix Inc
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

module Memory = struct

  type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  external alloc_pages: int -> t = "caml_alloc_pages"

  let get n =
    if n < 1
    then raise (Invalid_argument "The number of page should be greater or equal to 1")
    else
      try alloc_pages n with _ ->
        Gc.compact ();
        try alloc_pages n with _ -> raise Out_of_memory

  let page_size = 4096

  let alloc bytes =
    (* round up to next PAGE_SIZE *)
    let pages = (bytes + page_size - 1) / page_size in
    (* but round-up 0 pages to 0 *)
    let pages = max pages 1 in
    let larger_than_we_need = get pages in
    Bigarray.Array1.sub larger_than_we_need 0 bytes

  let set_byte t i v = Bigarray.Array1.set t i v
end

module File = struct
  type t = Unix.file_descr

  external openfile: string -> int -> t = "stub_directio_openfile"

  let seek (t: t) offset =
    if (Int64.(mul (div offset 512L) 512L)) <> offset
    then failwith (Printf.sprintf "seek is not sector aligned: %Ld" offset);

    let (_: int64) = Unix.LargeFile.lseek t offset Unix.SEEK_SET in
    ()

  external really_read: t -> Memory.t -> int -> int -> unit = "stub_directio_really_read"

  external really_write: t -> Memory.t -> int -> int -> unit = "stub_directio_really_write"
end

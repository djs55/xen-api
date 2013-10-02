/*
 * Copyright (C) 2012-2013 Citrix Inc
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
 */
#define _GNU_SOURCE

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <malloc.h>


#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>

#define PAGE_SIZE 4096

/* ocaml/ocaml/unixsupport.c */
extern void uerror(char *cmdname, value cmdarg);

CAMLprim value stub_directio_openfile(value filename, value mode){
  CAMLparam2(filename, mode);
  CAMLlocal1(result);
  int fd;

  const char *filename_c = strdup(String_val(filename));

  enter_blocking_section();
  fd = open(filename_c, O_RDWR | O_DIRECT, Int_val(mode));
  leave_blocking_section();

  free((void*)filename_c);

  if (fd == -1) uerror("open", filename);

  CAMLreturn(Val_int(fd));
}

static void really_read(int fd, void *aligned_buffer, int len){
  int n;

  while (len > 0) {
    n = read(fd, aligned_buffer, len);
    if (n == 0) caml_failwith("short read");
    len -= n;
    aligned_buffer += n;  
  }
}

static void really_write(int fd, void *aligned_buffer, int len){
  int n;

  while (len > 0) {
    enter_blocking_section();
    n = write(fd, aligned_buffer, len);
    leave_blocking_section();
    if (n == 0) caml_failwith("short write");
    len -= n;
    aligned_buffer += n;
  }
}

/* external really_read: t -> Memory.buf -> int -> int -> unit = "stub_directio_really_read" */
CAMLprim value stub_directio_really_read(value fd, value ba, value ofs, value len){
  CAMLparam4(fd, ba, ofs, len);
  int fd_c = Int_val(fd);
  int ofs_c = Int_val(ofs);
  int len_c = Int_val(len);
  void *aligned_buffer = Caml_ba_data_val(ba);
  void *tmp;

  if (ofs_c == 0){
    really_read(fd_c, aligned_buffer, len_c);
  } else {
    /* inefficient hack: create a temporary buffer */
    tmp = memalign(len_c, PAGE_SIZE);
    if (tmp == NULL) caml_failwith("memalign");
    memcpy(tmp, aligned_buffer + ofs_c, len_c);
    really_read(fd_c, tmp, len_c);
    free(tmp);
  }

  CAMLreturn(Val_unit);
}

/* external really_write: t -> Memory.buf -> int -> int -> unit = "stub_directio_really_write" */
CAMLprim value stub_directio_really_write(value fd, value ba, value ofs, value len){

  CAMLparam4(fd, ba, ofs, len);
  int fd_c = Int_val(fd);
  int ofs_c = Int_val(ofs);
  int len_c = Int_val(len);
  void *aligned_buffer = Caml_ba_data_val(ba);
  void *tmp;

  if (ofs_c == 0){
    really_write(fd_c, aligned_buffer, len_c);
  } else {
    /* inefficient hack: create a temporary buffer */
    tmp = memalign(len_c, PAGE_SIZE);
    if (tmp == NULL) caml_failwith("memalign");
    memcpy(tmp, aligned_buffer + ofs_c, len_c);
    really_write(fd_c, tmp, len_c);
    free(tmp);
  }

  CAMLreturn(Val_unit);
}

/* From mirage:
   https://github.com/mirage/mirage/commit/3d7aace5eb162f99b79b21d1784f87ce26fec8b7
*/

/* Allocate a page-aligned bigarray of length [n_pages] pages.
   Since CAML_BA_MANAGED is set the bigarray C finaliser will
   call free() whenever all sub-bigarrays are unreachable.
 */
CAMLprim value
caml_alloc_pages(value n_pages)
{
  CAMLparam1(n_pages);
  size_t len = Int_val(n_pages) * PAGE_SIZE;
  /* If the allocation fails, return None. The ocaml layer will
     be able to trigger a full GC which just might run finalizers
     of unused bigarrays which will free some memory. */
  void* block = memalign(PAGE_SIZE, len);

  if (block == NULL) {
    caml_failwith("memalign");
  }
  CAMLreturn(caml_ba_alloc_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT | CAML_BA_MANAGED, 1, block, len));
}


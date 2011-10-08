open Xenops_interface

type context

val make_context: unit -> context

val query: context -> unit -> query_result

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

type 'a update =
	| Modify of 'a
	| Delete of 'a

module UpdateRecorder = functor(Ord: Map.OrderedType) -> struct
	(* Map of thing -> last update counter *)
	module M = Map.Make(struct
		type t = Ord.t
		let compare = compare
	end)

	type id = int

	type t = {
		map: int M.t;
		next: id
	}

	let empty = {
		map = M.empty;
		next = 0;
	}

	let add x t = {
		map = M.add x t.next t.map;
		next = t.next + 1
	}, t.next + 1

	let remove x t = {
		map = M.remove x t.map;
		next = t.next + 1
	}, t.next + 1

	let get from t =
		(* XXX: events for deleted things *)
		let before, after = M.partition (fun _ time -> time < from) t.map in
		M.fold (fun key v (acc, m) -> Modify key :: acc, max m v) after ([], from)
end

module Updates = struct
	open Threadext

	module U = UpdateRecorder(struct type t = Dynamic.id let compare = compare end)

	type id = U.id

	type t = {
		mutable u: U.t;
		c: Condition.t;
		m: Mutex.t;
	}

	let empty () = {
		u = U.empty;
		c = Condition.create ();
		m = Mutex.create ();
	}

	let get from t =
		Mutex.execute t.m
			(fun () ->
				let current = ref ([], from) in
				while fst !current = [] do
					current := U.get from t.u;
					if fst !current = [] then Condition.wait t.c t.m;
				done;
				!current
			)

	let add x t =
		Mutex.execute t.m
			(fun () ->
				let result, id = U.add x t.u in
				t.u <- result;
				Condition.signal t.c;
				id
			)

	let remove x t =
		Mutex.execute t.m
			(fun () ->
				let result, id = U.remove x t.u in
				t.u <- result;
				Condition.signal t.c;
				id
			)
end

module type S = sig
	module VM : sig
		val create: Vm.t -> unit
		val build: Vm.t -> Vbd.t list -> Vif.t list -> unit
		val create_device_model: Vm.t -> unit
		val destroy: Vm.t -> unit
		val pause: Vm.t -> unit
		val unpause: Vm.t -> unit

		val suspend: Vm.t -> disk -> unit
		val resume: Vm.t -> disk -> unit

		val get_state: Vm.t -> Vm.state
	end
	module VBD : sig
		val plug: Vm.id -> Vbd.t -> unit
		val unplug: Vm.id -> Vbd.t -> unit

		val get_state: Vm.id -> Vbd.t -> Vbd.state
	end
	module VIF : sig
		val plug: Vm.id -> Vif.t -> unit
		val unplug: Vm.id -> Vif.t -> unit

		val get_state: Vm.id -> Vif.t -> Vif.state
	end
	module UPDATES : sig
		val get: Updates.id -> Dynamic.id update list * Updates.id
	end
	module DEBUG : sig
		val trigger: string -> string list -> unit
	end
end

(*
 * Copyright (C) 2011 Citrix Systems Inc.
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

type processor = Rpc.call -> Rpc.response

open Storage_interface

let plugins : (API.ref_SR, processor) Hashtbl.t = Hashtbl.create 10

let register sr m = Hashtbl.replace plugins sr m
let unregister sr = Hashtbl.remove plugins sr
(* This is the policy: *)
let of_sr sr = Hashtbl.find plugins (Ref.of_string sr)

open Fun

let multicast f = Hashtbl.fold (fun sr rpc acc -> (sr, f rpc) :: acc) plugins []

let partition = List.partition (success ++ snd) 

let choose x = snd(List.hd x)

let fail_or f results =
	let successes, errors = partition results in
	if errors <> [] then choose errors else f successes

module Mux = struct
	type context = unit
	module DP = struct
		let create context ~task ~id = id (* XXX: is this pointless? *)
		let destroy context ~task ~dp ~allow_leak =
			(* Tell each plugin about this *)
			fail_or choose (multicast (Client.DP.destroy ~task ~dp ~allow_leak))
		let diagnostics context () =
			let combine results = 
				let all = List.fold_left (fun acc (sr, result) -> 
					Printf.sprintf "For SR: %s" (Ref.string_of sr) :: (string_of_result result) :: acc) [] results in
				Success (String (String.concat "\n" all)) in
			fail_or combine (multicast (fun rpc -> Client.DP.diagnostics rpc ()))
	end
		
	module SR = struct
		let attach context ~task ~sr = Client.SR.attach (of_sr sr) ~task ~sr
		let detach context ~task ~sr = Client.SR.detach (of_sr sr) ~task ~sr
		let destroy context ~task ~sr = Client.SR.destroy (of_sr sr) ~task ~sr
		let list context ~task = 
			List.fold_left (fun acc (sr, list) -> list @ acc) [] (multicast (Client.SR.list ~task))
	end
	module VDI = struct
		let create context ~task ~sr ~name_label ~name_description ~virtual_size ~ty ~params = 
			Client.VDI.create (of_sr sr) ~task ~sr ~name_label ~name_description ~virtual_size ~ty ~params
		let stat context ~task ?dp ~sr ~vdi () = Client.VDI.stat (of_sr sr) ~task ?dp ~sr ~vdi ()
		let destroy context ~task ~sr ~vdi = Client.VDI.destroy (of_sr sr) ~task ~sr ~vdi
		let attach context ~task ~dp ~sr ~vdi ~read_write = Client.VDI.attach (of_sr sr) ~task ~dp ~sr ~vdi ~read_write
		let activate context ~task ~dp ~sr ~vdi = Client.VDI.activate (of_sr sr) ~task ~dp ~sr ~vdi
		let deactivate context ~task ~dp ~sr ~vdi = Client.VDI.deactivate (of_sr sr) ~task ~dp ~sr ~vdi
		let detach context ~task ~dp ~sr ~vdi = Client.VDI.detach (of_sr sr) ~task ~dp ~sr ~vdi
	end
end

module Server = Storage_interface.Server(Mux)

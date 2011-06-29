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
 * @group Storage
 *)
 
open Pervasiveext

module D=Debug.Debugger(struct let name="smint" end)
open D

type vdi_info = {
	vdi_info_uuid: string option;
	vdi_info_location: string;
}

let make_vdi_info ~location ?uuid () = 
  { vdi_info_uuid = uuid;
    vdi_info_location = location;
  }

(** Very primitive first attempt at a set of backend capabilities *)
type capability =
    | Sr_create | Sr_delete | Sr_attach | Sr_detach | Sr_scan | Sr_probe | Sr_update 
	| Sr_supports_local_caching
    | Vdi_create | Vdi_delete | Vdi_attach | Vdi_detach
    | Vdi_clone | Vdi_snapshot | Vdi_resize | Vdi_activate | Vdi_deactivate
    | Vdi_update | Vdi_introduce 
    | Vdi_resize_online
    | Vdi_generate_config
	| Vdi_reset_on_boot

let string_to_capability_table = [
    "SR_PROBE",       Sr_probe;
    "SR_UPDATE",      Sr_update;
	"SR_SUPPORTS_LOCAL_CACHING", Sr_supports_local_caching;
    "VDI_CREATE",     Vdi_create;
    "VDI_DELETE",     Vdi_delete;
    "VDI_ATTACH",     Vdi_attach;
    "VDI_DETACH",     Vdi_detach; 
    "VDI_RESIZE",     Vdi_resize;
    "VDI_RESIZE_ONLINE",Vdi_resize_online;
    "VDI_CLONE",      Vdi_clone;
    "VDI_SNAPSHOT",   Vdi_snapshot;
    "VDI_ACTIVATE",   Vdi_activate;
    "VDI_DEACTIVATE", Vdi_deactivate;
    "VDI_UPDATE",     Vdi_update;
    "VDI_INTRODUCE",  Vdi_introduce;
    "VDI_GENERATE_CONFIG", Vdi_generate_config;
	"VDI_RESET_ON_BOOT", Vdi_reset_on_boot;
]
let capability_to_string_table = List.map (fun (x, y) -> y, x) string_to_capability_table

let lookup tbl x = if List.mem_assoc x tbl then Some (List.assoc x tbl) else None

let capability_of_string = lookup string_to_capability_table
let string_of_capability = lookup capability_to_string_table

let all_capabilities =
  [ Sr_create; Sr_delete; Sr_attach; Sr_detach; Sr_scan; Sr_probe; Sr_update;
    Sr_supports_local_caching; 
    Vdi_create; Vdi_delete; Vdi_attach; Vdi_detach;
    Vdi_clone; Vdi_resize; Vdi_activate; Vdi_deactivate;
    Vdi_update; Vdi_introduce;
    Vdi_resize_online
  ]

type sm_type = Executable | Daemon

type sr_driver_info = {
        sr_driver_filename: string;
	sr_driver_name: string;
	sr_driver_description: string;
	sr_driver_vendor: string;
	sr_driver_copyright: string;
	sr_driver_version: string;
	sr_driver_required_api_version: string;
	sr_driver_capabilities: capability list;
	sr_driver_text_capabilities: string list;
	sr_driver_configuration: (string * string) list;
	sr_driver_type : sm_type;
}

exception Backend_missing_field of string
exception Backend_report_error of (int * string)
exception Command_failed of (int * string * string (*stdout*) * string (*stderr*))
exception Command_killed of (int * string * string (*stdout*) * string (*stderr*))
exception Missing_field of string

exception Not_implemented_in_backend (* Raised by clone at least *)

exception Sr_not_empty
exception Vdi_in_use
exception Device_in_use


type request = {
	uri: string;
}

let string_of_request x = Printf.sprintf "{ uri = %s }" x.uri

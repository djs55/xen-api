open Lwt
open Cohttp

let debug fmt = Logging.debug "starter" fmt
let warn  fmt = Logging.warn  "starter" fmt
let error fmt = Logging.error "starter" fmt

let message_logger = Logging.create 512
let message conn_id session (fmt: (_,_,_,_) format4) =
    Printf.ksprintf message_logger.Logging.push ("[%3d] [%s]" ^^ fmt) conn_id (match session with None -> "None" | Some x -> x)

let rec logging_thread logger =
    lwt lines = Logging.get logger in
	lwt () = Lwt_list.iter_s
            (fun x ->
                lwt () = Lwt_log.log ~logger:!Lwt_log.default ~level:Lwt_log.Notice x in
				return ()
			) lines in
	logging_thread logger

let startswith prefix x = String.length x >= (String.length prefix) && (String.sub x 0 (String.length prefix) = prefix)

let port = ref 8080

open Cohttp_lwt_unix

let make_server () =
	debug "Started server on port %d" !port;
    let (_: 'a) = logging_thread Logging.logger in
    let (_: 'a) = logging_thread message_logger in

  	(* (Response.t * Body.t) Lwt.t *)
	let callback conn_id ?body req =
		lwt body = match body with
			| None -> return None
			| Some b ->
				lwt s = Body.string_of_body (Some b) in
				return (Some s) in
		match (Request.meth req), (Request.path req) with
			| `GET, _ ->
				Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"hello" ()
			| _, _ ->
				Cohttp_lwt_unix.Server.respond_not_found ~uri:(Request.uri req) ()
	in
	let conn_closed conn_id () = () in

	let (_: 'a Lwt.t) = logging_thread Logging.logger in

	let config = { Cohttp_lwt_unix.Server.callback; conn_closed } in
	server ~address:"0.0.0.0" ~port:!port config
    
let _ =
	Arg.parse [
		"-port", Arg.Set_int port, "port to listen on";
	] (fun x -> Printf.fprintf stderr "Ignoring: %s" x)
		"Run VDI performance test";

	Lwt_unix.run (make_server ()) 


open Lwt
open Cohttp
open Cohttp_lwt_unix
open Xen_api
open Xen_api_lwt_unix

let uri = ref "http://127.0.0.1/"
let username = ref "root"
let password = ref "password"

let debug fmt = Logging.debug "starter" fmt
let info  fmt = Logging.info  "starter" fmt

let warn  fmt = Logging.warn  "starter" fmt
let error fmt = Logging.error "starter" fmt

let rec logging_thread logger =
    lwt lines = Logging.get logger in
	lwt () = Lwt_list.iter_s
            (fun x ->
                lwt () = Lwt_log.log ~logger:!Lwt_log.default ~level:Lwt_log.Notice x in
				return ()
			) lines in
	logging_thread logger

let port = ref 8080


let mac_address_regex = Re.compile(Re_posix.re "[0-9A-F][0-9A-F]-[0-9A-F][0-9A-F]-[0-9A-F][0-9A-F]-[0-9A-F][0-9A-F]-[0-9A-F][0-9A-F]-[0-9A-F][0-9A-F]")

(* table of MAC to VM name label *)
let mac_to_vm = Hashtbl.create 128

let login_wakeup = Hashtbl.create 128

let make_anonymous_name =
	let counter = ref 0 in
	fun () ->
		incr counter;
		Printf.sprintf "anonymous-%d" !counter

let process_start = Unix.gettimeofday ()

let xmlrpc rpc =
	match rpc.Rpc.name, rpc.Rpc.params with
		| "hello", [ Rpc.String payload ] ->
			debug "hello %s" payload;
			let macs = Re.get_all (Re.exec mac_address_regex payload) in
			(* Switch to lowercase with colons *)
			let macs = Array.map
				(fun x ->
					let m = String.lowercase x in
					for i = 0 to String.length m - 1 do
						if m.[i] = '-' then m.[i] <- ':'
					done;
					m
				) macs in
			let vm = List.fold_left (fun acc mac -> match acc with
				| Some x -> Some x
				| None -> if Hashtbl.mem mac_to_vm mac then Some (Hashtbl.find mac_to_vm mac) else None
			) None (Array.to_list macs) in
			let name = match vm with
				| None -> make_anonymous_name ()
				| Some x -> x in
			if Hashtbl.mem login_wakeup name
			then Lwt.wakeup_later (Hashtbl.find login_wakeup name) ();
			Rpc.String name
		| "report_error", [ Rpc.String vm; Rpc.String message ] ->
			error "ERROR %s %s" vm message;
			Rpc.String "sorry to hear it"
		| "login_vsi_results", [ Rpc.String vm; Rpc.Dict results ] ->
			debug "login_vsi_results %s %s" vm (Jsonrpc.to_string (Rpc.Dict results));
			List.iter
				(function
					| (_, Rpc.Enum ts) ->
						List.iter
							(function
								| Rpc.String line ->
									let bits = Array.of_list(Re_str.(split_delim (regexp "[,]") line)) in
									(* Overwrite the hostname with the VM name label *)
									bits.(3) <- vm;
									let line = String.concat "," (Array.to_list bits) in
									info "VSI %s" line
								| _ -> debug "failed to parse loginvsi results"
							) ts
					| _ -> debug "failed to parse loginvsi results"
				) results;
			Rpc.String "whatever"
		| _, _ ->
			Rpc.String "unknown method"

let to_string v =
	let b = Buffer.create 256 in
	Buffer.add_string b "<?xml version=\"1.0\"?><methodResponse><params><param>";
	Buffer.add_string b (Xmlrpc.to_string v);
	Buffer.add_string b "</param></params></methodResponse>";
	Buffer.contents b

let make_server () =
	debug "Started server on port %d" !port;

  	(* (Response.t * Body.t) Lwt.t *)
	let callback conn_id ?body req =
		lwt body = match body with
			| None -> return ""
			| Some b ->
				lwt s = Body.string_of_body (Some b) in
				return s in
		match (Request.meth req), (Request.path req) with
			| `GET, _ ->
				Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"hello" ()
			| `POST, _ ->
				(* debug "POST [%s]" body; *)
				let request = Xmlrpc.call_of_string body in
				let response = to_string (xmlrpc request) in
				Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:response ()
			| _, _ ->
				Cohttp_lwt_unix.Server.respond_not_found ~uri:(Request.uri req) ()
	in
	let conn_closed conn_id () = () in

	let config = { Cohttp_lwt_unix.Server.callback; conn_closed } in
	server ~address:"0.0.0.0" ~port:!port config

let start_vms () =
    let rpc = make !uri in
    lwt session_id = Session.login_with_password rpc !username !password "1.0" in
	try_lwt
        lwt vms = VM.get_all_records rpc session_id in
		lwt () = Lwt_list.iter_s
            (fun (vm, vm_rec) ->
				if List.mem_assoc "test" vm_rec.API.vM_other_config then begin
					debug "Processing VM %s\n" vm_rec.API.vM_name_label;
					let t, u = Lwt.task () in
					Hashtbl.replace login_wakeup vm_rec.API.vM_name_label u;
					lwt () = Lwt_list.iter_s
						(fun vif ->
							lwt mac = VIF.get_MAC rpc session_id vif in
							debug "VM %s has MAC %s" vm_rec.API.vM_name_label mac;
							Hashtbl.replace mac_to_vm mac vm_rec.API.vM_name_label;
							return ()
						) vm_rec.API.vM_VIFs in
					let now = Unix.gettimeofday () in
					lwt () =
							try_lwt
								lwt () = VM.start rpc session_id vm false false in
								info "START %s %.0f %.0f" vm_rec.API.vM_name_label (now -. process_start) (Unix.gettimeofday () -. process_start);
								t
							with
								| Api_errors.Server_error(code, params) ->
									error "ERROR %s %s %s" vm_rec.API.vM_name_label code (String.concat " " params);
									return ()
								| e ->
									error "ERROR %s %s" vm_rec.API.vM_name_label (Printexc.to_string e);
									return () in
					return ()
				end else return ()
            ) vms in
		return ()
    finally
        Session.logout rpc session_id

let main () =
    let (_: 'a) = logging_thread Logging.logger in

	(* Start the XMLRPC server *)
	let (server: unit Lwt.t) = make_server () in

	lwt () = start_vms () in
	lwt () = server in
	return ()


let _ =
	Arg.parse [
        "-uri", Arg.Set_string uri, (Printf.sprintf "URI of server to connect to (default %s)" !uri);
        "-u", Arg.Set_string username, (Printf.sprintf "Username to log in with (default %s)" !username);
        "-pw", Arg.Set_string password, (Printf.sprintf "Password to log in with (default %s)" !password);
		"-port", Arg.Set_int port, "port to listen on";
	] (fun x -> Printf.fprintf stderr "Ignoring: %s" x)
		"Run VDI performance test";

	Lwt_unix.run (main ()) 


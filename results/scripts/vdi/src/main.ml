open Lwt
open Cohttp
open Xen_api
open Xen_api_lwt_unix

let uri = ref "http://127.0.0.1/"
let username = ref "root"
let password = ref "password"

let debug fmt = Logging.debug "starter" fmt
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

let startswith prefix x = String.length x >= (String.length prefix) && (String.sub x 0 (String.length prefix) = prefix)

let port = ref 8080

open Cohttp_lwt_unix

(*
<?xml version='1.0'?>
<methodCall>
<methodName>hello</methodName>
<params>
<param>
<value><string>
Physical Address    Transport Name                                            
=================== ==========================================================
72-D0-B7-94-68-C7   \Device\Tcpip_{59D14E22-9CA7-41C2-846C-AC942C229C24}      
</string></value>
</param>
</params>
</methodCall>

<?xml version='1.0'?>
<methodCall>
<methodName>report_error</methodName>
<params>
<param>
<value><string>OpaqueRef:cfa12479-2352-2c9b-ea37-b8109744acdd</string></value>
</param>
<param>
<value><string>&lt;ProtocolError for 10.81.64.61:8080/RPC2: 404 Not Found&gt;</string></value>
</param>
</params>
</methodCall>
*)
let mac_address_regex = Re.compile(Re_posix.re "[0-9A-F][0-9A-F]-[0-9A-F][0-9A-F]-[0-9A-F][0-9A-F]-[0-9A-F][0-9A-F]-[0-9A-F][0-9A-F]-[0-9A-F][0-9A-F]")

(* table of MAC to VM reference *)
let mac_to_vm = Hashtbl.create 128

let make_anonymous_name =
	let counter = ref 0 in
	fun () ->
		incr counter;
		Printf.sprintf "anonymous-%d" !counter

let xmlrpc body =
	let rpc = Xmlrpc.call_of_string body in
	match rpc.Rpc.name, rpc.Rpc.params with
		| "hello", [ Rpc.String payload ] ->
			let macs = Re.get_all (Re.exec mac_address_regex payload) in
			let vm = List.fold_left (fun acc mac -> match acc with
				| Some x -> Some x
				| None -> if Hashtbl.mem mac_to_vm mac then Some (Hashtbl.find mac_to_vm mac) else None
			) None (Array.to_list macs) in
			begin match vm with
				| None -> make_anonymous_name ()
				| Some x -> x
			end
		| _ ->
			"Not found"

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
				debug "POST [%s]" body;
				Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:(xmlrpc body) ()
			| _, _ ->
				Cohttp_lwt_unix.Server.respond_not_found ~uri:(Request.uri req) ()
	in
	let conn_closed conn_id () = () in

	let (_: 'a Lwt.t) = logging_thread Logging.logger in

	let config = { Cohttp_lwt_unix.Server.callback; conn_closed } in
	server ~address:"0.0.0.0" ~port:!port config

let exn_to_string = function
        | Api_errors.Server_error(code, params) ->
                Printf.sprintf "%s %s" code (String.concat " " params)
        | e -> Printexc.to_string e

let start_vms () =
    let rpc = make !uri in
    lwt session_id = Session.login_with_password rpc !username !password "1.0" in
	try_lwt
        lwt vms = VM.get_all_records rpc session_id in
		lwt () = Lwt_list.iter_s
            (fun (vm, vm_rec) ->
				if List.mem_assoc "test" vm_rec.API.vM_other_config then begin
					debug "Processing VM %s\n" vm_rec.API.vM_name_label;
					lwt () = Lwt_list.iter_s
						(fun vif ->
							lwt mac = VIF.get_MAC rpc session_id vif in
							debug "VM %s has MAC %s" vm_rec.API.vM_name_label mac;
							Hashtbl.replace mac_to_vm mac vm_rec.API.vM_name_label;
							return ()
						) vm_rec.API.vM_VIFs in
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


(** Multi-client server example.

    Clients can increment a shared counter or read its current value.

    Build with: ocamlfind ocamlopt -package lwt,lwt.unix -linkpkg -o counter-server ./counter-server.ml
 *)


open Lwt

let counter = ref 0

let listen_address = Unix.inet_addr_loopback
let port = 9000
let backlog = 10

let () = Lwt_log.add_rule "*" Lwt_log.Info

let handle_message msg =
  match msg with
  | "read" -> string_of_int !counter
  | "inc"  -> counter := succ !counter; "Counter has been incremented"
  | _      -> "Unknown command"

let rec handle_connection ic oc () =
  Lwt_io.read_line_opt ic
  >>= fun msg ->
  match msg with
  | Some msg ->
     handle_message msg
     |> Lwt_io.write_line oc
     >>= handle_connection ic oc
  | None -> Lwt_log.info "Connection closed" >>= return

let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  handle_connection ic oc ();
  Lwt_log.info "New connection" >>= return

let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  bind sock @@ ADDR_INET (listen_address, port);
  listen sock backlog;
  sock

let create_server sock =
  let rec serve () =
    Lwt_unix.accept sock >>= accept_connection >>= serve
  in serve

let () =
  create_socket ()
  |> create_server
  |> fun s -> Lwt_main.run @@ s ()

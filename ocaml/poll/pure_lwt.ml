open Lwt
open Lwt_unix
module React = Lwt_react
open Batteries

exception Break_event_loop

let add_handler f fd =
  let in_chan = Lwt_io.of_fd ~mode:Lwt_io.Input fd in 
  let io_loop () =
    Lwt_unix.wrap_syscall Lwt_unix.Read fd (fun () ->
                            try
                              Lwt_io.read_line in_chan
                              >|= f
                              |> (fun _ -> ())
                            with
                            | Unix.Unix_error (Unix.EAGAIN, _, _) -> raise Lwt_unix.Retry
                            | Unix.Unix_error (Unix.EINTR, _, _) ->  raise Break_event_loop)
  in
  let rec loop () =
    Lwt.catch
      io_loop
      (function
       | Break_event_loop -> loop ()
       | exn -> Lwt.fail exn)
  in
  loop

  (*
let add_handler_loop f fd =
  let in_chan = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let rec loop () =
    Lwt_io.read_line in_chan
    >>= (fun c -> (f c); return ())
    >>= loop
  in
  loop
   *)
  
let () =
  let open React in
  let s, s_set = E.create () in
  let lp = Lwt_unix.stdin
          |> add_handler s_set 
  in
  let printer = E.map print_endline s in 
  Lwt_main.run (lp ())
  
(*

let make_loop () =
    let open React in
    let s, s_set = E.create () in
    let rec loop () =
      Lwt_unix.sleep 1. >>= fun () ->
      s_set "Test"; loop ()
    in
    loop, s;;

let l, s = make_loop ();;

let printer = E.map print_endline s;;

Lwt_main.run (l ());;
 *)

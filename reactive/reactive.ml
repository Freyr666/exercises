let pr_time t =
  let tm = Unix.localtime t in
  Printf.printf "\x1B[8D%02d:%02d:%02d%!"
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

open Lwt_react;;

let create fsend frecv interval =
  let seconds, run =
    let e, send = E.create () in
    let run () =
      while true do send (fsend ()); Unix.sleep interval done
    in
    e, run
  in
  let _ = E.map frecv seconds in
  run

let () =
  let r1 = create Unix.gettimeofday pr_time 1 in
  let r2 = create (fun () -> "Hello!") print_endline 3 in
  (E.select [r1;r2]) ()

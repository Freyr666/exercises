open Lwt

let mk_call f =
  let ic, oc = Lwt_io.pipe () in
  let rec loop ic =
    Lwt_io.read_char ic
    >>= (fun c -> (f c); return ic)
    >>= loop
  in
  let _ = loop ic in
  oc

let () = print_endline "hello"

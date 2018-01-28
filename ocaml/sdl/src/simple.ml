open Containers
open Tsdl

let (>>=) = Result.(>>=)
   
let main () =
  Sdl.init Sdl.Init.video
  >>= fun () ->
  Sdl.create_window_and_renderer ~h:1024 ~w:768 Sdl.Window.opengl
  >>= fun (win, rend) ->
  Sdl.render_draw_lines rend
    [(Sdl.Point.create ~x:100 ~y:100);
     (Sdl.Point.create ~x:600 ~y:100);
     (Sdl.Point.create ~x:350 ~y:500)]
  >>= fun () ->
  Sdl.delay 3000l;
  Sdl.destroy_window win;
  Sdl.quit ();
  Ok ()

let () =
  match main () with
  | Ok () -> ()
  | Error (`Msg e) -> print_endline e  

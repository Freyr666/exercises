open Gg
open Vg

let aspect = 1.618
let size = Size2.v (aspect *. 100.) 100.
let view = Box2.v P2.o (Size2.v aspect 1.)
let image = I.const (Color.v_srgb 0.314 0.784 0.471)

let main _ =
  let d = Dom_html.window ##. document in
  let a =
    let a = Dom_html.createA d in
    a ##. title := Js.string "Download PNG file";
    a ##. href  := Js.string "#";
    a ## (setAttribute (Js.string "download") (Js.string "min_htmlc.png"));
    Dom.appendChild (d ##. body) a; a
  in
  let c =
    let c = Dom_html.createCanvas d in
    Dom.appendChild a c; c
  in
  let r = Vgr.create (Vgr_htmlc.target c) `Other in
  ignore (Vgr.render r (`Image (size, view, image)));
  ignore (Vgr.render r `End);
  a ##. href := (c ## toDataURL);
  Js._false

let () = Dom_html.window ##. onload := Dom_html.handler main

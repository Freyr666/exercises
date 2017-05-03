(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the BSD2 License.
 * See the file COPYING for details.
 *)

let (%) f g x = f (g x)

let () = Random.self_init () (* get a seed for random numbers *)

(** Squares and tiles *)
type provenance = { shift : int; value : int }

(* A tile is represented as its value. *)
type tile = int * provenance list

(* An unoccupied square is represented as None.
   A square occupied by a tile t is represented as Some t. *)
type square = tile option

(* The provenance of a square is a list of the previous positions and
   values of the current occupants.

   A freshly-populated square has provenance
     [].

   A square unchanged by the last move has provenance
     [{shift = 0; value = v}].

   A square occupied by a shifted tile has provenance
     [{shift = s; value = v}].

   A square occupied by combining two tiles has provenance
     [{shift = s1; value = v}; {shift = s2; value = v}].
*)

(* A board is a list of lists of squares *)
type row = square list
type board = row list

type move = L | R | U | D

module type Solution = sig
  val is_square_2048: square -> bool
  val is_complete_row: row -> bool
  val is_board_winning : board -> bool
  val is_valid_move : move -> board -> bool
  val insert_square : square -> board -> board option
  val shift_left_helper: row -> row -> row
  val shift_board : move -> board -> board
  val is_game_over : board -> bool
  val square_provenances: square -> provenance list
end

let empty = None
let t2 = Some (2,[{value = 2;shift = 0}])
let t4 = Some (4,[{value = 4;shift = 0}])
let t8 = Some (8,[{value = 8;shift = 0}])
let t16 = Some (16,[{value = 16;shift = 0}])
let t32 = Some (32,[{value = 32;shift = 0}])
let t64 = Some (64,[{value = 64;shift = 0}])
let t128 = Some (128,[{value = 128;shift = 0}])
let t256 = Some (256,[{value = 256;shift = 0}])
let t512 = Some (512,[{value = 512;shift = 0}])
let t1024 = Some (1024,[{value = 1024;shift = 0}])
let t2048 = Some (2048,[{value = 2048;shift = 0}])

(* The value of the occupant of a square, if the square is occupied. *)
let square_value (sq : square) =
  match sq with
  | None -> None
  | Some (v,_) -> Some v

let string_of_square = function
| Some (s,_) -> string_of_int s
| None -> " "

(* Select a tile to insert.  Returns t4 10% of the time and t2 90% of the time. *)
let new_square () : square =
  match Random.int 10 with
  | 0 -> t4
  | _ -> t2

(** Boards *)

let create_board () =
  [ [empty; t2   ; empty; empty];
    [empty; empty; empty; empty];
    [empty; empty; empty; empty];
    [empty; empty; empty; t2   ]; ]

let board_size = Utils.listlist_dims
let fold_board = Utils.fold_listlisti

(** Moves *)

module Make (S: Solution) = struct

  include S

  (** High-level interface. *)
  let game_move (mv : move) (b : board) : board =
    let clear_prov : (square -> square) = function
      | None -> None
      | Some (v,prov) -> Some (v,[{shift = 0; value = v}])
    in
    let clear_board_prov =
      List.map (fun r -> List.map clear_prov r)
    in
    let b = clear_board_prov b in
    if is_valid_move mv b then begin
        let b' = S.shift_board mv b in
        match S.insert_square (new_square ()) b' with
        | None -> b'
        | Some b'' -> b''
      end
    else b

end

module Default = struct

  type tile = int

  let is_square_2048 : (square -> bool) = function
    | Some (2048,_) -> true
    | _             -> false

  let is_board_winning (b : board) =
    List.exists (fun row -> List.exists is_square_2048 row) b

  let rec shift_left_helper (r : row) (new_row: row) : row =
    let inc_prov prov = List.map (fun p -> { p with shift = succ p.shift }) prov
    in 
    let inc_shift r : row = List.map
                              (function
                               | None -> None
                               | Some (v, prov) ->
                                  Some (v, inc_prov prov))
                              r
    in
    match r with
    | [] -> new_row
    | (None)::tl -> shift_left_helper (inc_shift tl) (None :: new_row)
    | (Some x)::(None)::tl -> shift_left_helper ((Some x)::(inc_shift tl)) (None :: new_row)
    | (Some (x,x_prov))::(Some (y,y_prov))::tl ->
       if x = y
       then
         let nval = x + y in
         let nprov = x_prov @ (inc_prov y_prov) in 
         (Some (nval,nprov))::(shift_left_helper (inc_shift tl) (None :: new_row)) 
       else
         (Some (x,x_prov))::(shift_left_helper ((Some (y,y_prov))::tl) new_row)
    | (Some x)::tl -> (Some x)::(shift_left_helper tl new_row)

  let shift_left (r : row) = shift_left_helper r []

  let shift_right (r : row) = List.rev @@ shift_left @@ List.rev r

  let rec shift_board (mv : move) (b : board) : board =
    match mv with
    | L -> List.map shift_left b
    | R -> List.map shift_right b
    | U -> Utils.transpose @@ List.map shift_left @@ Utils.transpose b
    | D -> Utils.transpose @@ List.map shift_right @@ Utils.transpose b

  let insert_square (sq : square) (b : board) : board option =
    let swap (new_val : square) (old_val : square) =
      match old_val with
      | Some _ -> None
      | None   -> if Random.bool () then Some new_val else None
    in
    Utils.replace_one (Utils.replace_one @@ swap sq) b

  let rec is_complete_row : (row -> bool) = function
    | [] -> true
    | (None)::tl -> false
    | (Some _)::[] -> true
    | (Some _)::(None)::tl -> false
    | (Some x)::(Some y)::tl -> if x = y then false else is_complete_row tl

  let is_game_over (b : board) =
    let r = List.exists (fun row -> not @@ is_complete_row row) b in
    let c = List.exists (fun row -> not @@ is_complete_row row) (Utils.transpose b) in
    not (r || c)

  let square_provenances (sq : square) =
    match sq with
    | None -> []
    | Some (_, pr) -> pr

  let is_valid_move (mv : move) (b : board) =
    let rec left_valid : (row -> bool) = function
      | [] -> false
      | (Some (x,_))::(Some (y,pr))::tl -> if x = y then true else left_valid @@ (Some (y,pr))::tl
      | (Some _)::tl -> left_valid tl
      | (None)::(Some _)::tl -> true
      | (None)::tl -> left_valid tl
    in
    match mv with
    | L -> List.exists left_valid b
    | R -> List.exists (left_valid % List.rev) b
    | U -> List.exists left_valid (Utils.transpose b)
    | D -> List.exists (left_valid % List.rev) (Utils.transpose b)

end

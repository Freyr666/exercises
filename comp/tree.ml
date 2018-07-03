type id = string

type binop = Plus | Minus | Times | Div

type stm = Compound of stm * stm
         | Assign   of id * exp
         | Print    of exp list
and exp = Id of id
        | Num of int
        | Op of exp * binop * exp
        | Eseq of stm * exp

let prog =
  Compound (Assign ("a", Op (Num 5, Plus, Num 3)),
            Compound (Assign ("b",
                              Eseq (Print [Id "a"; Op (Id "a", Minus, Num 1)],
                                    Op (Num 10, Times, Id "a"))),
                      Print [ Id "b" ]))

let max a b = if a < b then b else a
  
let rec maxargs : stm -> int = function
  | Compound (l,r) -> max (maxargs l) (maxargs r)
  | Assign (_,e) -> maxargs_exp e
  | Print  lst   ->
     let len = List.length lst in
     let exps = List.fold_left (fun acc x -> max acc (maxargs_exp x)) 0 lst in
     max len exps
and maxargs_exp = function
  | Id _ | Num _ -> 0
  | Op (l,_,r) -> max (maxargs_exp l) (maxargs_exp r)
  | Eseq (s,e) -> max (maxargs s) (maxargs_exp e)

let _ = assert (maxargs prog = 2)

module SMap = Map.Make(String)
      
let interp : stm -> unit = fun prog ->
  let rec interp_stm tbl = function
    | Compound (l, r) -> let t = interp_stm tbl l in interp_stm t r
    | Assign (v,e) -> let e = interp_exp tbl e in SMap.add v e tbl
    | Print lst ->
       let lst = List.map (interp_exp tbl) lst in
       List.iter (Printf.printf " %d") lst;
       print_newline ();
       tbl
  and interp_exp tbl = function
    | Num x -> x
    | Id id -> SMap.find id tbl
    | Eseq (s,e) -> let t = interp_stm tbl s in interp_exp t e
    | Op (l,o,r) ->
       let l, r = interp_exp tbl l, interp_exp tbl r in
       match o with
       | Plus -> l + r
       | Minus -> l - r
       | Div -> l / r
       | Times -> l * r
  in
  ignore @@ interp_stm SMap.empty prog

let () = interp prog

module Tree = struct

  type key = string

  type 'a t = Leaf | Tree of key * 'a * 'a t * 'a t

  let empty = Leaf

  let rec insert tree k v =
    match tree with
    | Leaf -> Tree (k, v, Leaf, Leaf)
    | Tree (tk, tv, l, r) ->
       if tk < k then Tree (tk, tv, l, insert r k v)
       else if tk > k then Tree (tk, tv, insert l k v, r)
       else Tree (tk, v, l, r)

  let rec find tree k =
    match tree with
    | Leaf -> None
    | Tree (tk, tv, l, r) ->
       if tk < k then find r k
       else if tk > k then find l k
       else Some tv
  
end

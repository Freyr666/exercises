
module type S = sig
  type t = Free of string
         | Bound of int
         | Abs of string * t
         | Apply of t * t
  val abstract : int -> string -> t -> t
  val abs_list : string list -> t -> t
  val apply_list : t -> t list -> t
  val subst : int -> t -> t -> t
  val inst : (string, t) Hashtbl.t -> t -> t 
end

module V : S = struct

  type t = Free of string
         | Bound of int
         | Abs of string * t
         | Apply of t * t

  let rec abstract level name = function
    | Free f -> if f = name then Bound level else Free f
    | Bound _ as b -> b
    | Abs (n, t) -> Abs (n, abstract (succ level) name t)
    | Apply (f, t) -> Apply (abstract level name f, abstract level name t)

  let abs_list names t =
    List.fold_right (fun name term ->
        Abs(name, abstract 0 name term))
      names
      t

  let apply_list f args =
    List.fold_left (fun f arg ->
        Apply(f,arg))
      f
      args

  let rec shift i d u =
    if i = 0 then u
    else match u with
         | Free _ as t -> t
         | Bound j -> if j >= d then Bound (j + i) else Bound j
         | Abs(n, t) -> Abs(n, shift i (d + 1) t)
         | Apply(f, t) -> Apply(shift i d f, shift i d t)
  
  let rec subst i u = function
    | Free _ as t -> t
    | Bound j as t ->
       if j < i then t
       else if j = i then shift i 0 u
       else Bound (j - 1)
    | Abs(n,t) -> Abs(n, subst (succ i) u t)
    | Apply(f,t) -> Apply(subst i u f, subst i u t)

  let rec inst env = function
    | Free name ->
       begin try inst env (Hashtbl.find env name)
             with _ -> Free name
       end
    | Bound _ as t -> t
    | Abs(n, t) -> Abs(n, inst env t)
    | Apply(f, t) -> Apply (inst env f, inst env t)
  
end

module Reduce : sig
  val eval : V.t -> V.t
  val by_value : V.t -> V.t
  val head_nf : V.t -> V.t
  val by_name : V.t -> V.t
end = struct

  let rec eval = function
    | V.Apply(f,arg) ->
       begin match eval f with
       | V.Abs(_,t) -> eval (V.subst 0 (eval arg) t)
       | f -> V.Apply(f, eval arg)
       end
    | t -> t

  let rec by_value t = bodies (eval t)
  and bodies = function
    | V.Abs(n,t) -> V.Abs(n, by_value t)
    | V.Apply(f,t) -> V.Apply(bodies f, bodies t)
    | t -> t

  let rec head_nf = function
    | V.Abs(n,t) -> V.Abs(n, head_nf t)
    | V.Apply(f,arg) ->
       begin match head_nf f with
       | V.Abs(_, t) -> head_nf (V.subst 0 arg t)
       | f' -> V.Apply(f', arg)
       end
    | t -> t

  let rec by_name t = args (head_nf t)
  and args = function
    | V.Abs(n, t) -> V.Abs(n, args t)
    | V.Apply(f,arg) -> V.Apply(args f, by_name arg)
    | t -> t
  
end

module ReadWrite : sig
  val read : string -> V.t
  val show : V.t -> unit
end = struct

  module LamKey : Lex.KEYWORD = struct
    let alphas = []
    let symbols = ["\\"; "."; "("; ")"]
    let comment_sign = ("(*", "*)")
  end

  module LamParse = Parse.Make(Lex.Make(LamKey))

  let read =
    let make_lambda ((b,bs), t) = V.abs_list (b::bs) t in
    let make_apply (f, args) = V.apply_list f args in
    let open LamParse in
    let rec term tokens =
      either
        ~left:("\\" %-- id -- repeat id -- "." %-- term >> make_lambda)
        ~right:(atom -- repeat atom >> make_apply)
        tokens
    and atom tokens =
      either
        ~left:(id >> (fun x -> V.Free x))
        ~right:("(" %-- term -- (key ")") >> fst)
        tokens
    in fun s ->
       reader term (Stream.of_string s)

  let show =
    let rec free_vars = function
      | V.Free n -> [n]
      | V.Bound _ -> []
      | V.Abs(_,t) -> free_vars t
      | V.Apply(f,t) -> free_vars f @ free_vars t
    in
    let rec rename free_vars a =
      if List.exists (fun x -> x = a) free_vars
      then rename free_vars (a ^ "'")
      else a
    in
    let rec strip acc = function
      | V.Abs (n,t) ->
         let b = rename (free_vars t) n in
         strip (b::acc) (V.subst 0 (V.Free b) t)
      | t -> (List.rev acc, t)
    in
    let strip_abs t = strip [] t in
    let rec term = function
      | V.Free n -> Pretty.str n
      | V.Bound _ -> Pretty.str "??UNMATCHED INDEX??"
      | V.Abs _ as t ->
         let (bs, u) = strip_abs t in
         let str = "\\ " ^ List.fold_right (fun v acc -> v ^ " " ^ acc) bs ". " in
         Pretty.blo 0 [Pretty.str str; term u]
      | t -> Pretty.blo 0 (applic t)
    and applic = function
      | V.Apply (t, u) -> applic t @ [Pretty.brk 1; atom u]
      | t -> [atom t]
    and atom = function
      | V.Free n -> Pretty.str n
      | t -> Pretty.blo 1 [Pretty.str "("; term t; Pretty.str ")"]
    in fun t ->
       Pretty.show Stdlib.stdout (term t) 50
  
end

let std_env =
  let t = Hashtbl.create 100 in
  List.iter
    (fun (name, term) -> Hashtbl.add t name (ReadWrite.read term))
    [ "true", "\\x y.x"
    ; "false", "\\x y.y"
    ; "if", "\\p x y. p x y"
    ; "pair", "\\x y f. f x y" (* Pairs *)
    ; "fst", "\\p. p true"
    ; "snd", "\\p. p false"
    ; "0", "\\f x. x" (* Natural numbers *)
    ; "succ", "\\n f x. f (n f x)"
    ; "zerop", "\\n. n (\\x. false) true"
    ; "1", "succ 0"
    ; "2", "succ 1"
    ; "3", "succ 2"
    ; "4", "succ 3"
    ; "5", "succ 4"
    ; "6", "succ 5"
    ; "7", "succ 6"
    ; "8", "succ 7"
    ; "9", "succ 8"
    ; "add", "\\m n f x. m f (n f x)"
    ; "mult", "\\m n f. m (n f)"
    ; "expt", "\\m n f x. m n f x"
    ; "prefn", "\\f p. pair (f (fst p)) (fst p)"
    ; "pre", "\\n f x. snd (n (prefn f) (pair x x))"
    ; "sub", "\\m n. n pre m"
    ; "nil", "\\z. z" (* Lists *)
    ; "cons", "\\x y. pair false (pair x y)"
    ; "null", "fst"
    ; "hd", "\\z. fst (snd z)"
    ; "tl", "\\z. snd (snd z)"
    ];
  t

let () =
  let rec repl () =
    print_string "=> ";
    let s = read_line () in
    let term = V.inst std_env (ReadWrite.read s) in
    let reduced = Reduce.by_value term in
    print_string "-- ";
    ReadWrite.show reduced;
    repl ()
  in repl ()

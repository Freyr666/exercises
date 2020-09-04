
type ('a, 'b) multifun = 'a -> 'b Seq.t

let (--) l r x =
  Seq.flat_map r (l x)

let (||) l r x =
  let lres = l x in
  if lres () = Seq.Nil
  then r x
  else lres

let (|@|) l r x =
  Seq.append (l x) (r x)

let all x = Seq.return x

let no _ = Seq.empty

let attempt tac = tac || all

let rec repeat tac x = (tac -- repeat tac || all) x

let seq_hd seq =
  match seq () with
  | Seq.Cons(h,_) -> h
  | _ -> failwith "Empty Seq"

let repeat_determ tac x =
  let rec rep x =
    try rep (seq_hd (tac x))
    with _ -> x
  in Seq.return (rep x)

let rec depth_first pred tac x =
  (if pred x then all
   else tac -- depth_first pred tac) x

let depth_iter pred n tac x =
  let next x = List.of_seq (tac x) in
  let rec dfs i sf y () =
    if i < 0 then sf ()
    else if i < n && pred y
    then Seq.cons y ((List.fold_left (dfs (i-1)) sf (next y)) ())
    else List.fold_left (dfs (i-1)) sf (next y) ()
  in
  let rec deepen k = dfs k (fun () -> deepen (k + n)) x () in
  deepen 0

let orelse l r u = l u || r u

let first_fun ts = List.fold_left orelse (fun _ -> no) ts

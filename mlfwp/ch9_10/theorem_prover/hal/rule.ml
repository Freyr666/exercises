
type state = State of Fol.goal list * Fol.formulae * int

type tactic = state -> state Seq.t

let main (State (_,p,_)) = p

let subgoals (State (gs,_,_)) = gs

let initial p = State ([([],[p])],p,0)

let final (State (gs,_,_)) = List.length gs = 0

let splice_goals gs newgs n =
  let rec take n = function
    | [] -> []
    | h::tl ->
       if n <= 0 then []
       else h::(take (pred n) tl)
  in
  let rec drop n = function
    | [] -> []
    | h::tl ->
       if n <= 0 then h::tl
       else drop (pred n) tl
  in
  (take (pred n) gs)
  @ newgs
  @ (drop n gs)

let prop_rule goal_fun i (State(gs,p,n)) =
  try let gs' = splice_goals gs (goal_fun (List.nth gs (pred i))) i in
      List.to_seq [State(gs',p,n)]
  with _ -> Seq.empty

let basic =
  prop_rule (fun (ps,qs) ->
      if List.exists (fun p -> List.exists (fun q -> p = q) qs) ps
      then []
      else failwith "Not a basic goal")

let unify =

  let rec unifiable = function
    | [], _ -> Seq.empty
    | p::ps, qs ->
       let rec find = function
         | [] -> unifiable (ps, qs)
         | (q::qs) ->
            try Seq.cons (Unify.atoms p q) (fun () -> find qs ())
            with Unify.Failed -> find qs
       in find qs
  in
  
  let inst env gs p n =
    State (List.map (Unify.inst_goal env) gs, Unify.inst_formulae env p, n)
  in
  
  fun i (State(gs,p,n)) ->
  try let (ps,qs) = List.nth gs (pred i) in
      let next env = inst env (splice_goals gs [] i) p n in
      Seq.map next (unifiable (ps, qs))
  with _ -> Seq.empty

let split_rel rel qs =
  let rec get = function
    | [] -> raise Not_found
    | Fol.Rel(n, ps)::_ when n = rel -> ps
    | _::qs -> get qs
  and del = function
    | [] -> []
    | Fol.Rel(n, _)::qs when n = rel -> qs
    | q::qs -> q::(del qs)
  in
  (get qs, del qs)

let prop_left prop left_fun =
  prop_rule (fun (ps,qs) -> left_fun (split_rel prop ps, qs))

let prop_right prop right_fun =
  prop_rule (fun (ps,qs) -> right_fun (ps, split_rel prop qs))

let conj_left =
  prop_left "&" (function
      | (([p1;p2], ps), qs) -> [(p1::p2::ps, qs)]
      | _ -> assert false)

let conj_right =
  prop_right "&" (function
      | (ps, ([q1;q2],qs)) -> [(ps, q1::qs); (ps, q2::qs)]
      | _ -> assert false)

let disj_left =
  prop_left "|" (function
      | (([p1;p2], ps), qs) -> [(p1::ps, qs); (p2::ps, qs)]
      | _ -> assert false)

let disj_right =
  prop_right "|" (function
      | (ps, ([q1;q2], qs)) -> [(ps, q1::q2::qs)]
      | _ -> assert false)

let imp_left =
  prop_left "-->" (function
      | (([left;right],ps), qs) -> [(ps, left::qs); (right::ps, qs)]
      | _ -> assert false)

let imp_right =
  prop_right "-->" (function
      | (ps, ([left;right], qs)) -> [(left::ps, right::qs)]
      | _ -> assert false)

let neg_left =
  prop_left "~" (function
      | (([p],ps), qs) -> [(ps, p::qs)]
      | _ -> assert false)

let neg_right =
  prop_right "~" (function
      | (ps, ([q], qs)) -> [(q::ps, qs)]
      | _ -> assert false)

let iff_left =
  prop_left "<->" (function
      | (([left;right],ps), qs) -> [(left::right::ps, qs); (ps, left::right::qs)]
      | _ -> assert false)

let iff_right =
  prop_right "<->" (function
      | (ps, ([left;right], qs)) -> [(left::ps, right::qs); (right::ps, left::qs)]
      | _ -> assert false)

let gensym n =
  let letter n =
    String.sub "abcdefghijklmnopqrstuvwxyz" n 1
  in
  let rec gen n =
    if n < 26 then "_" ^ letter n
    else gen (n / 26) ^ letter (n mod 26)
  in gen n

let split_quant kind qs =
  let rec get = function
    | [] -> raise Not_found
    | (Fol.Quant(k,_,_) as q)::_ when k = kind -> q
    | _::qs -> get qs
  and del = function
    | [] -> []
    | Fol.Quant(k,_,_)::qs when k = kind -> qs
    | q::qs -> q::(del qs)
  in
  (get qs, del qs)

let quant_rule goal_fun i (State(gs,p,n)) =
  try let gs' = splice_goals gs (goal_fun ((List.nth gs (pred i)), (gensym n))) i in
      List.to_seq [State(gs', p, succ n)]
  with _ -> Seq.empty

let forall_left =
  quant_rule (fun ((ps,qs), b) ->
      match split_quant "ALL" ps with
      | (Fol.Quant(_,_,p) as qf, ps') ->
         let px = Fol.subst 0 (Fol.Var b) p in
         [ (px::ps' @ [qf], qs) ]
      | _ -> assert false)

let forall_right =
  quant_rule (fun ((ps,qs), b) ->
      match split_quant "ALL" qs with
      | (Fol.Quant(_,_,q), qs') ->
         let vars = Fol.goal_vars [] (ps, qs) in
         let qx = Fol.subst 0 (Fol.Param(b, vars)) q in
         [(ps, qx::qs')]
      | _ -> assert false)

let exists_left =
  quant_rule (fun ((ps,qs), b) ->
      match split_quant "EX" ps with
      | (Fol.Quant(_,_,p), ps') ->
         let vars = Fol.goal_vars [] (ps, qs) in
         let px = Fol.subst 0 (Fol.Param(b, vars)) p in
         [(px::ps', qs)]
      | _ -> assert false)

let exists_right =
  quant_rule (fun ((ps,qs), b) ->
      match split_quant "EX" qs with
      | (Fol.Quant(_,_,q) as qf, qs') ->
         let qx = Fol.subst 0 (Fol.Var b) q in
         [ (ps, qx::qs' @ [qf]) ]
      | _ -> assert false)
         

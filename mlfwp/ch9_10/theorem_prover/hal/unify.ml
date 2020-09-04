
exception Failed

module Env = Map.Make(String)

let rec unify_lists env l r =
  let rec chase = function
    | Fol.Var name ->
       (try chase (Env.find name env)
        with Not_found -> Fol.Var name)
    | t -> t
  in
  let rec occurs name = function
    | Fol.Fun (_,ts) -> occurs_in_list name ts
    | Fol.Param (_,vs) -> occurs_in_list name (List.map (fun x -> Fol.Var x) vs)
    | Fol.Var vname ->
       name = vname
       || (try occurs name (Env.find name env)
           with Not_found -> false)
    | _ -> false
  and occurs_in_list name = List.exists (occurs name)
  and unify = function
    | Fol.Var name, t | t, Fol.Var name ->
       if t = Fol.Var name
       then env
       else if occurs name t
       then raise Failed
       else Env.update name (fun _ -> Some t) env
    | Fol.Param (a,_), Fol.Param (b,_) ->
       if a = b then env
       else raise Failed
    | Fol.Fun (a,ts), Fol.Fun (b,us) ->
       if a = b
       then unify_lst (ts, us)
       else raise Failed
    | _ -> raise Failed
  and unify_lst = function
    | [], [] -> env
    | t::ts, u::us -> unify_lists (unify (chase t, chase u)) ts us
    | _ -> raise Failed
  in unify_lst (l, r)

let atoms l r =
  match l, r with
  | Fol.Pred (a,ts), Fol.Pred (b, us) ->
     if a = b
     then unify_lists Env.empty ts us
     else raise Failed
  | _ -> raise Failed

let rec inst_term env = function
  | Fol.Var name ->
     (try inst_term env (Env.find name env)
      with Not_found -> Fol.Var name)
  | Fol.Param (name, bs) ->
     Fol.Param (name,
                List.fold_left Fol.term_vars [] (List.map (fun v ->
                                                     inst_term env (Fol.Var v)) bs))
  | Fol.Fun (name, ts) -> Fol.Fun (name, List.map (inst_term env) ts)
  | t -> t

let rec inst_formulae env = function
  | Fol.Pred (n, ts) -> Fol.Pred (n, List.map (inst_term env) ts)
  | Fol.Rel (n,fs) -> Fol.Rel (n, List.map (inst_formulae env) fs)
  | Fol.Quant (kind,var,p) -> Fol.Quant (kind, var, inst_formulae env p)

let inst_goal env (ps, qs) =
  (List.map (inst_formulae env) ps, List.map (inst_formulae env) qs)

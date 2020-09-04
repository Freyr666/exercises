
type term = Var of string
          | Param of string * string list
          | Bound of int
          | Fun of string * term list

and formulae = Pred of string * term list
             | Rel of string * formulae list
             | Quant of string * string * formulae

and goal = formulae list * formulae list

let rec replace ~what ~by t =
  if what = t then by
  else match t with
       | Fun (n, terms) -> Fun (n, List.map (replace ~what ~by) terms)
       | _ -> t

let rel_precedence = function
  | "~" -> 4
  | "&" -> 3
  | "|" -> 2
  | "<->" -> 1
  | "-->" -> 1
  | _ -> -1

let rec abstract level term = function
  | Pred(name, terms) -> Pred(name, List.map (replace ~what:term ~by:(Bound level)) terms)
  | Rel(name, forms) -> Rel(name, List.map (abstract level term) forms)
  | Quant(kind, var, form) -> Quant(kind, var, abstract (succ level) term form)

let rec subst level term = function
  | Pred(name, terms) -> Pred(name, List.map (replace ~what:(Bound level) ~by:term) terms)
  | Rel(name, forms) -> Rel(name, List.map (subst level term) forms)
  | Quant(kind, var, form) -> Quant(kind, var, subst (succ level) term form)

module StringSet = Set.Make(String)

let rec term_vars_set set = function
  | Var name -> StringSet.add name set
  | Fun (_, terms) -> List.fold_left term_vars_set set terms
  | _ -> set

let rec form_vars_set set = function
  | Pred (_, terms) -> List.fold_left term_vars_set set terms
  | Rel (_, forms) -> List.fold_left form_vars_set set forms
  | Quant (_,_,f) -> form_vars_set set f

let term_vars vars term =
  let set = StringSet.of_list vars in
  StringSet.elements (term_vars_set set term)

let goal_vars vars (ps, qs) =
  let set = StringSet.of_list vars in
  StringSet.elements
    (List.fold_left
       form_vars_set 
       (List.fold_left form_vars_set set ps)
       qs)

let rec term_params params = function
  | Param (n,ts) -> (n,ts)::params
  | Fun (_, terms) -> List.fold_left term_params params terms
  | _ -> params

let rec form_params params = function
  | Pred (_, terms) -> List.fold_left term_params params terms
  | Rel (_, forms) -> List.fold_left form_params params forms
  | Quant (_,_,f) -> form_params params f

let goal_params params (ps, qs) =
  List.fold_left
    form_params
    (List.fold_left form_params params ps)
    qs

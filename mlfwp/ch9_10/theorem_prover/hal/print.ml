
let enclose exp = Pretty.blo 1 [Pretty.str "("; exp; Pretty.str ")"]

let rec commas = function
  | [] -> []
  | exp::exps -> Pretty.str ","
                 :: Pretty.brk 1
                 :: exp
                 :: commas exps

let list = function
  | [] -> assert false
  | h::tl -> Pretty.blo 0 (h :: commas tl)

let rec term = function
  | Fol.Param (name, _) -> Pretty.str name
  | Fol.Var id -> Pretty.str ("?" ^ id)
  | Fol.Bound _ -> Pretty.str "??UNMATCHED INDEX??"
  | Fol.Fun (name, ts) -> Pretty.blo 0 [Pretty.str name; args ts]
and args = function
  | [] -> Pretty.str ""
  | ts -> enclose (list (List.map term ts))

let rec formp k = function
  | Fol.Pred (name, ts) ->
     Pretty.blo 0 [Pretty.str name; args ts]
  | Fol.Rel ("~", [p]) ->
     Pretty.blo 0 [Pretty.str "~"; formp (Fol.rel_precedence "~") p]
  | Fol.Rel (bin, [p;q]) ->
     let pf = formp (max (Fol.rel_precedence bin) k) in
     let exp = Pretty.blo 0 [ pf p; Pretty.str (" " ^ bin)
                              ; Pretty.brk 1; pf q]
     in
     if Fol.rel_precedence bin <= k
     then (enclose exp)
     else exp
  | Fol.Quant (kind, var, p) ->
     let q = Fol.subst 0 (Fol.Fun(var,[])) p in
     let exp = Pretty.blo 2 [ Pretty.str (kind ^ " " ^ var ^ ".")
                            ; Pretty.brk 1
                            ; formp 0 q ]
     in
     if k > 0
     then enclose exp
     else exp
  | _ -> Pretty.str "??UNKNOWN FORMULAE??"

let form_list = function
  | [] -> Pretty.str "empty"
  | lst -> list (List.map (formp 0) lst)

let form p = Pretty.show Stdlib.stdout (formp 0 p) 50

let goal n (ps, qs) =
  Pretty.show Stdlib.stdout
    (Pretty.blo 4 [ Pretty.str (" " ^ Int.to_string n ^ ". ")
                  ; form_list ps
                  ; Pretty.brk 2
                  ; Pretty.str "|- "
                  ; form_list qs ])
    50

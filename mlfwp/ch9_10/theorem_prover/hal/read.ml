
module FolKeys : Lex.KEYWORD = struct
  let alphas = ["ALL"; "EX" ]
  let symbols = ["("; ")"; "."; ","; "?"; "~"; "&"; "|"; "<->"; "-->"; "|-"]
  let comment_sign = ("(*", "*)")
end

module FolParse = Parse.Make(Lex.Make(FolKeys))

open FolParse

let read =
  let make_quant ((kind,var), term) =
    Fol.Quant(kind, var, Fol.abstract 0 (Fol.Fun(var,[])) term)
    (* Fun of no terms is just a const *)
  in
  let make_bin_rel name p q =
    Fol.Rel (name, [p; q])
  in
  let make_neg p = Fol.Rel ("~", [p]) in
  let make_pred (name, terms) = Fol.Pred(name, terms) in
  let make_fun (id, terms) = Fol.Fun(id,terms) in
  let make_var id = Fol.Var id in

  (* Grammar *)
  let list ph tokens = (ph -- repeat ("," %-- ph) >> (fun (h,tl) -> h::tl)) tokens in
  
  let pack ph tokens =
    either
      ~left:("(" %-- list ph -- key ")" >> fst)
      ~right:empty
      tokens
  in
  
  let rec term tokens =
    either
      ~left:(id -- pack term >> make_fun)
      ~right:("?" %-- id >> make_var)
      tokens
  in

  let rec form tokens =
    either
      ~left:(key "ALL" -- id -- "." %-- form >> make_quant)
      ~right:(fun toks ->
        either
          ~left:(key "EX" -- id -- "." %-- form >> make_quant)
          ~right:(infixes primary Fol.rel_precedence make_bin_rel)
          toks)
      tokens

  and primary tokens =
    either
      ~left:("~" %-- primary >> make_neg)
      ~right:(fun toks ->
        either
          ~left:("(" %-- form -- key ")" >> fst)
          ~right:(id -- pack term >> make_pred)
          toks)
      tokens
  in
  fun s ->
  reader form (Stream.of_string s)

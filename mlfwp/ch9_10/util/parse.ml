
module type PARSE = sig

  exception Syntax_error of string
  
  type token

  (** Takes a single id token *)
  val id : token list -> string * token list

  (** Takes a single key token of given value *)
  val key : string -> token list -> string * token list

  (** Returns an empty list paired with original token list *)
  val empty : token list -> 'a list * token list

  (** Returns either left or right term *)
  val either : left:('a -> 'b) -> right:('a -> 'b) -> 'a -> 'b

  (** !!parser is same as parser except that if parser
      rejects the tokens than the entire parse fails *)
  val (!!) : (token list -> 'a * token list) -> token list -> 'a * token list

  (** parse1 -- parse2 consecutively parses phrase1 and phrase2 
      and returns a pair *)
  val (--) : (token list -> 'a * token list)
             -> (token list -> 'b * token list)
             -> token list
             -> ('a * 'b) * token list

  (** "s %-- parse" same as "key s -- parse", except it returns only phrase parsed
      by parse *)
  val (%--) : string -> (token list -> 'a * token list) -> token list -> 'a * token list

  (** "parse >> f" is equivalent of "let v, tl = parse ... in f v, tl" *)
  val (>>) : (token list -> 'a * token list)
             -> ('a -> 'b)
             -> token list
             -> 'b * token list

  val repeat : (token list -> 'a * token list) -> token list -> 'a list * token list

  val infixes : (token list -> 'a * token list)
                -> (string -> int)
                -> (string -> 'a -> 'a -> 'a)
                -> token list
                -> 'a * token list

  val reader : (token list -> 'a * token list) -> char Stream.t -> 'a
  
end

module Make (L : Lex.LEXICAL) : PARSE with type token := L.token = struct
  
  exception Syntax_error of string

  let id = function
    | (L.Id s)::tl -> (s, tl)
    | _ -> raise (Syntax_error "Identifier expected")

  let key name = function
    | (L.Key k)::tl when k = name -> (k, tl)
    | _ -> raise (Syntax_error "Symbol expected")

  let empty tokens = ([], tokens)

  let either ~left ~right tokens =
    try left tokens
    with Syntax_error _ -> right tokens

  let (!!) ph tokens =
    try ph tokens
    with Syntax_error msg -> failwith ("Syntax error: " ^ msg)

  let (--) ph1 ph2 tokens =
    let (x, tokens2) = ph1 tokens in
    let (y, tokens3) = ph2 tokens2 in
    ((x, y), tokens3)

  let (>>) ph f tokens =
    let (x, tokens2) = ph tokens in
    (f x, tokens2)

  let (%--) name ph =
    key name -- !!ph >> snd

  let rec repeat ph tokens =
    either
      ~left:(ph -- repeat ph >> (fun (h,tl) -> h::tl))
      ~right:empty
      tokens

  let infixes ph prec_of apply =
    let rec over k tokens = next k (ph tokens)
    and next k = function
      | (x, L.Key(name)::tl) ->
         if prec_of name < k
         then (x, L.Key(name)::tl)
         else next k ((over (prec_of name) >> apply name x) tl)
      | res -> res
    in
    over 0

  let reader ph stream =
    match ph (L.scan stream) with
    | (x, []) -> x
    | _ -> raise (Syntax_error "Extra characters in phrase")
  
end


module type LEXICAL = sig
  
  (** A token is either an Identifier or
      one of the predefined keywords *)
  type token = Id of string | Key of string

  (** Scans the input and produces
      a corresponding list of tokens *)
  val scan : char Stream.t -> token list

end

module type KEYWORD = sig

  (** Alphas are alphanumeric keywords
      like "let" of "if" *)
  val alphas : string list

  (** Symbolic keywords like "(",
      ")" or "=" *)
  val symbols : string list

  (** Designates beginning and ending of
      the commented lines *)
  val comment_sign : string * string
end

module Make (K : KEYWORD) : LEXICAL = struct

  type token = Id of string | Key of string

  let (%%) f g x = f (g x)
  
  let alpha_tok a =
    if List.mem a K.alphas
    then Key a
    else Id a

  let is_alphanum char =
    '0' <= char && char <= '9'
    || 'a' <= char && char <= 'z'
    || 'A' <= char && char <= 'Z'

  let is_graph char =
    let code = Char.code char in
    code > 32 && code != 127 (* 0-31 and 127 are control chars, 32 is a space *)

  let is_punct char =
    is_graph char && not (is_alphanum char)

  let forall_string pred s =
    let seq = String.to_seq s in
    Seq.fold_left (fun acc x -> acc && (pred x)) true seq
  ;;
  
  assert (forall_string is_punct (fst K.comment_sign));;
  assert (forall_string is_punct (snd K.comment_sign));;
  
  let s char = String.make 1 char

  let maybe_comment char = (fst K.comment_sign).[0] = char

  let split_left pred stream =
    let rec loop acc s =
      match Stream.peek s with
      | Some char when pred char ->
         Stream.junk s;
         loop (char::acc) s
      | _ -> List.rev acc, s
    in
    let l, r = loop [] stream in
    String.of_seq (List.to_seq l), r

  let drop_left pred stream =
    let rec loop s =
      match Stream.peek s with
      | Some char when pred char ->
         Stream.junk s;
         loop s
      | _ -> s
    in
    loop stream

  let rec symbolic symb stream =
    match Stream.peek stream with
    | None -> (Key symb, stream)
    | Some char ->
       if List.mem symb K.symbols
          || not (is_punct char)
       then (Key symb, stream)
       else begin
           Stream.junk stream;
           symbolic (symb ^ (s char)) stream
         end

  let drop_comment stream =
    let beg, end_ = K.comment_sign in
    let peek_str s =
      String.length s
      |> (fun n -> Stream.npeek n stream)
      |> List.to_seq
      |> String.of_seq
    in
    let rec drop_until_end s =
      Stream.junk s;
      if not (peek_str end_ = end_)
      then drop_until_end s
      else String.iter (fun _ -> Stream.junk s) end_
    in
    if not (peek_str beg = beg)
    then false
    else begin
        drop_until_end stream;
        true
      end
  
  let rec scanning tokens stream =
    match Stream.peek stream with
    | None -> List.rev tokens
    | Some char when maybe_comment char && (drop_comment stream) -> (* Drop comment if it's the case *)
       scanning tokens stream
    | Some char when is_alphanum char -> (* Identifier or keyword *)
       let (id, ss2) = split_left is_alphanum stream in
       let tok = alpha_tok id in
       scanning (tok::tokens) ss2
    | Some char when is_punct char -> (* Special symbol *)
       Stream.junk stream;
       let (tok, ss2) = symbolic (s char) stream in
       scanning (tok::tokens) ss2
    | _ -> (* Ignoring spaces, line breaks, control chars *)
       scanning tokens (drop_left (not %% is_graph) stream)

  let scan stream =
    scanning [] stream
  
end


type t = String of string
       | Break of int
       | Block of t list * int * int

let length = function
  | String s -> String.length s
  | Break len -> len
  | Block (_,_,len) -> len

let blo indent es =
  let rec sum k = function
    | [] -> k
    | e::es -> sum (length e + k) es
  in Block (es, indent, sum 0 es)

let str s = String s

let brk i = Break i

let rec breakdist after = function
  | [] -> after
  | (String s)::es -> String.length s + breakdist after es
  | (Break _)::_ -> 0
  | (Block (_,_,len))::es -> len + breakdist after es

let show oc t margin =
  let space = ref margin in
  let blanks n =
    output_string oc (String.make n ' ');
    space := !space - n
  in
  let newline () =
    output_char oc '\n';
    space := margin
  in
  let rec printing blockspace after = function
    | [] -> ()
    | e::es ->
       begin match e with
       | String s ->
          output_string oc s;
          space := !space - (String.length s)
       | Break len ->
          if len + (breakdist after es) <= !space
          then blanks len
          else (newline (); blanks (margin - blockspace))
       | Block (es', indent, _) ->
          printing (!space - indent) (breakdist after es) es'
       end;
       printing blockspace after es
  in
  printing margin 0 [t];
  newline ()

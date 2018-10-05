module type FUNCTOR = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Loeb (F : FUNCTOR) = struct
  let rec f x = F.map (fun a -> a (lazy (f x))) x
end

module Dict = struct
  type 'a t = (string * 'a) list
  let map f (d : 'a t) : 'b t =
    List.map (fun (a, b) -> a, f b) d
  let get x (d : 'a t) = List.assoc x d
end

module Run = struct
  type 'a t = Nil | Cons of 'a * 'a t Lazy.t
  let cons h tl = Cons (h, tl)
  let rec append l r = match Lazy.force l with
    | Nil -> Lazy.force r
    | Cons (h, tl) -> Cons (h, lazy (append tl r))
  let rec take n = function
    | Nil -> if n = 0 then [] else failwith "End of stream"
    | Cons (h, lazy tl) -> if n = 0
                           then [h]
                           else h::(take (pred n) tl)
end
            
let parse d =
  let compile l env =
    List.fold_right (fun e r ->
        let tail = lazy r in
        match e with
        | `Lit c -> Run.cons c tail
        | `Var w -> Run.append (lazy (Dict.get w (Lazy.force env))) tail)
      l Run.Nil
  in
  let module L = Loeb(Dict) in
  L.f (Dict.map compile d)

let test = parse [ "a", [`Lit 'x'; `Var "b"]
                 ; "b", [`Lit 'y'; `Var "a"]
             ]

let _ = Run.take 6 (Dict.get "a" test)

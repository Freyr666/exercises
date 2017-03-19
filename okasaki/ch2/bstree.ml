open Core.Std

(* Done: 2.2, 2.3 *)
   
module type Set = sig
  type e
  type t
  val empty : t
  val insert : e -> t -> t
  val member : e -> t -> bool
end
   
module UnbalancedSet (C : Comparable) : (Set with type e = C.t) = struct
  type e = C.t
         
  type t = Empty
         | Tree of e * t * t

  let empty = Empty

  let member x s =
    let rec member' acc x =
      let open C in
      function
      | Empty -> x = acc
      | Tree (x',l,r) -> 
         if x <= x' (* ex 2.2 *)
         then member' x'  x l
         else member' acc x r
    in
    match s with
    | Empty -> false
    | Tree (x',_,_) -> member' x' x s

  let insert x s =
    let rec rinsert x = function
      | Empty         -> Tree (x, Empty, Empty)
      | Tree (x',l,r) as s ->
         (match C.compare x x' with
          | -1 -> Tree (x', (rinsert x l), r)
          | 1  -> Tree (x', l, (rinsert x r))
          | _  -> s)
    in 
    if member x s
    then s
    else rinsert x s
end

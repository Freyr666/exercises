module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module type Showable = sig
  type t
  val show : t -> string
end

module type Comp_show = sig
  include Comparable
  include Showable with type t := t
end

module type Heap = sig
  type e
  type t

  val empty      : t
  val is_empty   : t -> bool
  val insert     : e -> t -> t
  val merge      : t -> t -> t
  val find_min   : t -> e option
  val delete_min : t -> t

  val show       : t -> string
end

module LHeap (E : Comp_show) : (Heap with type e = E.t) = struct
  type e = E.t
  type t = Empty
         | Tree of int * e * t * t

  let rank = function
    | Empty -> 0
    | Tree (r,_,_,_) -> r
                 
  let make_t e l r =
    if rank l >= rank r
    then Tree (rank r + 1,e,l,r)
    else Tree (rank l + 1,e,r,l)

  let empty = Empty
  let is_empty = function
    | Empty -> true
    | _     -> false                 
    
  let rec merge l r =
    match (l,r) with
    | (Empty,h) | (h,Empty) -> h
    | (Tree (_,e1,a1,b1), Tree (_,e2,a2,b2)) ->
       if E.compare e1 e2 <= 0
       then make_t e1 a1 (merge b1 r)
       else make_t e2 a2 (merge l b2)

  let insert x h = merge (Tree (1,x,Empty,Empty)) h

  let find_min = function
    | Empty -> None
    | Tree (_,e,_,_) -> Some e

  let delete_min = function
    | Empty -> Empty
    | Tree (_,x,l,r) -> merge l r

  let rec show = function
    | Empty -> "_"
    | Tree (_,x,l,r) -> "<(" ^ E.show x ^ ") "
                        ^ show l ^ " " ^ show r ^ " >"
 
end 

                                                            (* TODO: all *)

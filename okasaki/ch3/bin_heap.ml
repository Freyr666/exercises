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

module BHeap (E : Comp_show) : (Heap with type e = E.t) = struct
  exception Ops
          
  type e = E.t
  type tree = Node of int * e * tree list
  type t = tree list

  let rank (Node (r,x,c)) = r

  let root (Node (r,x,c)) = x

  let nodes (Node (r,x,c)) = c

  let link (Node (r,x1,c1) as t1) (Node (_,x2,c2) as t2) =
    if E.compare x1 x2 <= 0
    then Node (r+1, x1, t1::c1)
    else Node (r+1, x2, t2::c2)

  let rec ins_tree t = function
    | [] -> [t]
    | t'::ts' as ts ->
       if rank t < rank t'
       then t::ts
       else ins_tree (link t t') ts'

  let rec rm_min_tree = function
    | []    -> raise Ops
    | [t]   -> (t,[])
    | t::ts ->
       let (t',ts') = rm_min_tree ts in
       if E.compare (root t) (root t') <= 0
       then (t,ts)
       else (t',t::ts')
      
  let empty = []
            
  let is_empty = function
    | [] -> true
    | _  -> false
          
  let insert x ts = ins_tree (Node (0,x,[])) ts

  let rec merge ts1 ts2 =
    match (ts1,ts2) with
    | (ts,[]) | ([],ts) -> ts
    | t1::ts1', t2::ts2' ->
       if rank t1 < rank t2
       then t1::(merge ts1' ts2)
       else if rank t1 > rank t2
       then t2::(merge ts1 ts2')
       else ins_tree (link t1 t2) (merge ts1' ts2')

  let find_min ts =
    let (t,_) = rm_min_tree ts in
    Some (root t)

  let delete_min ts =
    let (Node (_,_,ts1),ts2) = rm_min_tree ts in
    merge (List.rev ts1) ts2

  let rec show ts =
    List.fold_left (fun acc x -> acc ^ " " ^ (show @@ nodes x)) "<[" ts ^ "]>"
    
end 

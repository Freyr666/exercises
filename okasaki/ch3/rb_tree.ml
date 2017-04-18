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

module type Set = sig
  type e
  type t
  val empty : t
  val insert : e -> t -> t
  val member : e -> t -> bool
  val show   : e -> string
end

module Red_black_tree (C : Comp_show) : (Set with type e = C.t) = struct
  type e = C.t
         
  type color = R | B
  type t  = Empty
          | Tree of color * e * t * t

  let empty = Empty

  let rec member x = function
    | Empty -> false
    | Tree (_,y,a,b) ->
       let dif = C.compare x y in
       if dif < 0
       then member x a
       else if dif > 0
       then member x b
       else true

  let balance = function
    | (B,z,Tree(R,y,Tree(R,x,a,b),c),d) 
      | (B,z,Tree(R,x,a,Tree(R,y,b,c)),d) 
      | (B,x,a,Tree(R,z,Tree(R,y,b,c),d)) 
      | (B,x,a,Tree(R,y,b,Tree(R,z,c,d))) -> Tree(R,y,Tree(B,x,a,b),Tree(B,z,c,d))
    | (col,x,a,b) -> Tree (col,x,a,b)
       
  let insert x s =
    let rec ins = function
      | Empty -> Tree (R,x,Empty,Empty)
      | Tree (c,y,a,b) as s ->
         let dif = C.compare x y in
         if dif < 0
         then balance (c,y,ins a,b)
         else if dif > 0
         then balance (c,y,a,ins b)
         else s
    in
    match ins s with
    | Tree (_,y,a,b) -> Tree (B,y,a,b)
    | Empty -> Empty

    let show s = "todo"
end

module RB_int = Red_black_tree (struct type t = int let compare = (fun x y -> x - y) let show = string_of_int end)

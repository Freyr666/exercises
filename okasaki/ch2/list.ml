module type Stack = sig
  type 'a t
  val empty    : 'a t
  val is_empty : 'a t -> bool
  val cons     : 'a -> 'a t -> 'a t
  val head     : 'a t -> 'a
  val tail     : 'a t -> 'a t
end

module List : (Stack with type 'a t = 'a list) = struct
  exception Empty
  type 'a t = 'a list
  let empty = []
  let is_empty = function
    | [] -> true
    | _  -> false
  let cons x xs = x::xs
  let head = function
    | x::xs -> x
    | _     -> raise Empty
  let tail = function
    | x::xs -> xs
    | _     -> raise Empty
end 

let rec suffixes l =
  if List.is_empty l
  then List.empty
  else List.cons l (suffixes @@ List.tail l)

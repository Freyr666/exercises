let (!$) = Lazy.force

module type STREAM = sig
  type 'a stream_cell = Nil | Cons of 'a * 'a stream
  and 'a stream = 'a stream_cell Lazy.t

  val (++) : 'a stream -> 'a stream -> 'a stream  (* stream append *)
  val take : int -> 'a stream -> 'a stream
  val drop : int -> 'a stream -> 'a stream
  val reverse : 'a stream -> 'a stream
end

module Stream : STREAM = struct 
  type 'a stream_cell = Nil | Cons of 'a * 'a stream
  and  'a stream = 'a stream_cell Lazy.t

  let rec (++) s1 s2 =
    lazy (match s1 with
          | lazy Nil -> Lazy.force s2
          | lazy (Cons (x,xs)) -> Cons (x,xs ++ s2))

  let rec take n s =
    lazy (
        match n, s with
        | 0, _ -> Nil
        | _, lazy Nil -> Nil
        | _, lazy (Cons (x,tl)) -> Cons (x,take (n - 1) tl))

  let rec drop n s =
    lazy (
        match n, s with
        | 0, _ -> !$ s
        | _, lazy Nil -> Nil
        | _, lazy (Cons(x,xs)) -> !$ (drop (n - 1) xs))

  let rec reverse s =
    lazy (
        let rec reverse' acc s =
          match s with
          | lazy Nil -> Lazy.force acc
          | lazy (Cons(x,xs)) -> (reverse' (lazy (Cons (x, acc))) xs)
        in
        reverse' (lazy Nil) s)
end 

let s = lazy (Stream.Cons (1, lazy (Stream.Cons (2, lazy (Stream.Cons (3, lazy Stream.Nil))))))

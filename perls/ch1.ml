open Core_kernel.Std

let (|.|) = Fn.compose

module Infinite = struct
  type 'a t = Null
            | Cons of 'a * 'a t Lazy.t

  let rec of_list = function
    | [] -> Null
    | x::xs -> Cons (x, Lazy.from_fun
                          (fun () -> of_list xs))

  let hd = function
    | Null -> None
    | Cons (x,_) -> Some x

  let tl = function
    | Null -> None
    | Cons (_,xs) -> Some (Lazy.force xs)

  let rec nth n = function
    | Null -> None
    | Cons (x,xs) ->
       if n <= 0
       then Some x
       else nth (pred n) (Lazy.force xs)

  let make h tl = Cons (h, Lazy.from_fun tl)

  let rec map f = function
    | Null -> Null
    | Cons (x,xs) ->
       Cons (f x,
             Lazy.map xs (map f))

  let rec filter p = function
    | Null -> Null
    | Cons (x, xs) ->
       if p x
       then Cons (x, Lazy.map xs (filter p))
       else Lazy.force (Lazy.map xs (filter p))
end

let integer =
  let rec f s = Infinite.make s (fun () -> f (succ s))
  in 
  f 0

let primes = 
  let open Infinite in
  let rec sieve = function
    | Null -> Null
    | Cons (x,xs) ->
       if x <= 1
       then Cons (x, Lazy.map xs sieve)
       else Cons (x, Lazy.map (Lazy.map xs (filter (fun y -> y mod x <> 0)))
                              sieve)
  in sieve integer
                  
let partition p =
  List.partition_map ~f:(fun x ->
                       if p x
                       then `Fst x
                       else `Snd x)
   
let (--) a b = List.filter ~f:(fun x ->
                             not @@ List.exists ~f:((=) x) b)
                           a

let () =
  let l = [1;2;3] -- [1;3;4] in
  print_int @@ List.hd_exn l
;;

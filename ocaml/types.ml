type (_,_) eq = Refl : ('a, 'a) eq

type z
type 'a s = Stemp : 'a -> 'a s

type (_,_) le =
  | LeZ : (z,'a) le
  | LeS : ('a,'b) le -> ('a,'b s) le
  
type _ isnat =
  | Z : z isnat
  | S : 'a isnat -> 'a s isnat

type (_,_,_) add =
  | Add_base : 'a isnat -> (z, 'a, 'a) add
  | Add_step : ('a, 'b, 'c) add -> ('a s, 'b, 'c s) add

let rec lemma_add_l_s_pred :
          type a b p. (a, b s, p s) add -> (a, b, p) add
  =
  function
  | Add_base (S x) -> Add_base x
  | Add_step s -> (lemma_add_l_s_pred (Add_step s))

let rec theorem_add_comm :
          type a b p q . (a, b, p) add -> (b, a, q) add -> (p, q) eq
  =
  function
  | Add_base Z -> (function Add_base Z -> Refl)
  | Add_base (S y) -> (function Add_step s ->
                         let Refl = theorem_add_comm (Add_base y) s in
                         Refl)
  | Add_step s1 ->
     (function
      | Add_base (S x) ->
         let Refl = theorem_add_comm s1 (Add_base x) in Refl
      | Add_step s ->
         let Refl = theorem_add_comm s1 (lemma_add_l_s_pred (Add_step s))
         in Refl)

let rec lemma_add_strict :
          type a b m n. (a, b, m) add -> (a, b, n) add -> (m, n) eq
  =
  fun hm hn ->
  match hm, hn with
  | Add_base _, Add_base _ -> Refl
  | Add_step s1, Add_step s2 ->
     let Refl = lemma_add_strict s1 s2 in
     Refl
     
let rec theorem_add_assoc :
          type a b c ab bc m n.
               (a, b, ab) add -> (ab, c, m) add ->
               (b, c, bc) add -> (a, bc, n) add -> (m, n) eq
  =
  fun ab ab_c bc a_bc ->
  match ab, a_bc with
  | Add_base b', Add_base bc' ->
     let Refl = lemma_add_strict ab_c bc in
     Refl
  | Add_step ab', Add_step a_bc' ->
     let Add_step ab_c' = ab_c in
     let Refl = theorem_add_assoc ab' ab_c' bc a_bc' in
     Refl

type (_,_) list =
  | []   : (z,'a) list
  | (::) : ('a * ('l,'a) list) -> ('l s, 'a) list

let hd : type l. (l s, 'a) list -> 'a = function x::_ -> x

let tl : type l. (l s, 'a) list -> (l, 'a) list = function _::tl -> tl

let _ = assert (hd [1] = 1)

let rec map : type l a b. (a -> b) -> (l, a) list -> (l, b) list =
  fun f ->
  function
  | []    -> []
  | x::tl -> (f x) :: (map f tl)

let rec append : type m n l. (m, n, l) add -> (m, 'a) list -> (n, 'a) list -> (l, 'a) list =
  function
  | Add_base _ -> (fun [] l2 -> l2)
  | Add_step s -> (function x::tl -> fun l2 -> x :: (append s tl l2))

let _ =
  assert (tl @@ append (Add_step (Add_step (Add_base (S (S (S Z)))))) [1;2] [3;4;5] = [2;3;4;5])

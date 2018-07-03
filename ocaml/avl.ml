type z = Z : z
type 'a s = S : 'a -> 'a s

type (_,_,_) diff =
  | Less : ('a, 'a s, 'a s) diff
  | Same : ('a, 'a, 'a) diff
  | More : ('a s, 'a, 'a s) diff

type ('a, 'd) atree =
  | Empty : ('a, z) atree
  | Tree  : ('a, 'm) atree * 'a * ('a, 'n) atree
            * ('m, 'n, 'o) diff -> ('a, 'o s) atree

type ('a, _) pos_result =
  | PSameDepth : ('a, 'd) atree -> ('a, 'd) pos_result
  | Deeper : ('a, 'd s) atree -> ('a, 'd) pos_result

type ('a, _) neg_result =
  | NSameDepth : ('a, 'd s) atree -> ('a, 'd s) neg_result
  | Shallower : ('a, 'd) atree -> ('a, 'd s) neg_result

let rotate_left : type d. ('a, d) atree -> 'a -> ('a, d s s) atree -> ('a, d s s) pos_result =
  fun l v r ->
  let Tree (rl, rv, rr, diff) = r in
  match diff with
  | Less -> PSameDepth (Tree (Tree (l, v, rl, Same), rv, rr, Same))
  | Same -> Deeper (Tree (Tree (l, v, rl, Less), rv, rr, More))
  | More -> begin
      let Tree (rll, rlv, rlr, diffl) = rl in
      match diffl with
      | Less -> PSameDepth (Tree (Tree (l, v, rll, More), rlv, Tree (rlr, rv, rr, Same), Same))
      | Same -> PSameDepth (Tree (Tree (l, v, rll, Same), rlv, Tree (rlr, rv, rr, Same), Same))
      | More -> PSameDepth (Tree (Tree (l, v, rll, Same), rlv, Tree (rlr, rv, rr, Less), Same))
    end

let rotate_right : type d. ('a, d s s) atree -> 'a -> ('a, d) atree -> ('a, d s s) pos_result =
  fun l v r ->
  let Tree (ll, lv, lr, diff) = l in
  match diff with
  | More -> PSameDepth (Tree (ll, lv, (Tree (lr, v, r, Same)), Same))
  | Same -> Deeper (Tree (ll, lv, (Tree (lr, v, r, More)), Less))
  | Less -> begin
      let Tree (lrl, lrv, lrr, diffr) = lr in
      match diffr with
      | More -> PSameDepth (Tree ((Tree (ll, lv, lrl, Same)), lrv, (Tree (lrr, v, r, Less)), Same))
      | Same -> PSameDepth (Tree ((Tree (ll, lv, lrl, Same)), lrv, (Tree (lrr, v, r, Same)), Same))
      | Less -> PSameDepth (Tree ((Tree (ll, lv, lrl, More)), lrv, (Tree (lrr, v, r, Same)), Same))
    end

let rec insert : type d. ('a -> 'a -> int) -> 'a -> ('a, d) atree -> ('a, d) pos_result =
  fun cmp v t ->
  match t with
  | Empty -> Deeper (Tree (Empty, v, Empty, Same))
  | Tree (l, tv, r, diff) ->
     match cmp v tv with
     | x when x == 0 -> PSameDepth t
     | x when x < 0 -> begin
         match insert cmp v l with
         | PSameDepth t' -> PSameDepth (Tree (t', tv, r, diff))
         | Deeper t'     ->
            match diff with
            | More -> rotate_right t' tv r
            | Same -> Deeper (Tree (t', tv, r, More))
            | Less -> PSameDepth (Tree (t', tv, r, Same))
       end
     | x when x > 0 -> begin
         match insert cmp v r with
         | PSameDepth t' -> PSameDepth (Tree (l, tv, t', diff))
         | Deeper t'     ->
            match diff with
            | Less -> rotate_left l tv t'
            | Same -> Deeper (Tree (l, tv, t', Less))
            | More -> PSameDepth (Tree (l, tv, t', Same))
       end

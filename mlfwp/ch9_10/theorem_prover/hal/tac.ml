
open Rule
open Tactical

let safe =
  first_fun [ basic
            (* 1 subgoal *)
            ; conj_left ; disj_right ; imp_right ; neg_left ; neg_right ; exists_left ; forall_right
            (* 2 subgoals *)
            ; conj_right ; disj_left ; imp_left ; iff_left ; iff_right ]

let safe_steps i = (safe i) -- repeat_determ (safe i)

let quant i = (forall_left i -- attempt (exists_right i)) || exists_right i

let depth = depth_first final (safe_steps 1 || unify 1 || quant 1)

let step i = safe_steps i || (unify i |@| forall_left i |@| exists_right i)

let depth_it d = depth_iter final d (step 1)

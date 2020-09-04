
type state

type tactic = state -> state Seq.t

(** State's main goal *)
val main : state -> Fol.formulae

(** State's subgoals *)
val subgoals : state -> Fol.goal list

(** Builds the initial state *)
val initial : Fol.formulae -> state

(** Checks if the proof is over *)
val final : state -> bool

(* Tactics for basic inference rules *)

val basic : int -> tactic

val unify : int -> tactic

val conj_left : int -> tactic

val conj_right : int -> tactic

val disj_left : int -> tactic

val disj_right : int -> tactic

val imp_left : int -> tactic

val imp_right : int -> tactic

val neg_left : int -> tactic

val neg_right : int -> tactic

val iff_left : int -> tactic

val iff_right : int -> tactic

val forall_left : int -> tactic

val forall_right : int -> tactic

val exists_left : int -> tactic

val exists_right : int -> tactic

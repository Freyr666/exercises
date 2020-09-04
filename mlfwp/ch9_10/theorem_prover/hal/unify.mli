
exception Failed

module Env : Map.S with type key = string

(** [atoms form1 form2] tries to unify two atomic formulae, returning
    a dictionary from variable names to terms. Atomic formulae is
    a predicate applied to argument list, such as P(t1,t2,..,tn) *)
val atoms : Fol.formulae -> Fol.formulae -> Fol.term Env.t

val inst_term : Fol.term Env.t -> Fol.term -> Fol.term

val inst_formulae : Fol.term Env.t -> Fol.formulae -> Fol.formulae

val inst_goal : Fol.term Env.t -> Fol.goal -> Fol.goal

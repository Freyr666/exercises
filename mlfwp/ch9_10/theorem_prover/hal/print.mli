
(** [form formulae] prints the formulae to stdout *)
val form : Fol.formulae -> unit

(** [goal number g] prints the [number] of [g]
    and [g] itself *)
val goal : int -> Fol.goal -> unit

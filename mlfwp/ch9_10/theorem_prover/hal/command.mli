
(** [goal formulae] accepting [formulae]
    setting the stored proof state to the
    initial state for provided [formulae] *)
val goal : string -> unit

(** [by tactic] applies tactic to current state *)
val by : Rule.tactic -> unit

(** [undo ()] undoes the most recent tactic application *)
val undo : unit -> unit

(** [print state] prints state to strout *)
val print : Rule.state -> unit

(** [get_state ()] returns the current proof state *)
val get_state : unit -> Rule.state


type ('a, 'b) multifun = 'a -> 'b Seq.t

(** sequencing operator *)
val (--) : ('a, 'b) multifun -> ('b, 'c) multifun -> ('a, 'c) multifun

(** if first tactic fails, second one is applied *)
val (||) : ('a, 'b) multifun -> ('a, 'b) multifun -> ('a, 'b) multifun

(** applies both tactics and concatenates their sequences *)
val (|@|) : ('a, 'b) multifun -> ('a, 'b) multifun -> ('a, 'b) multifun

(** if applied, returns a singleton sequence *)
val all : ('a, 'a) multifun

(** if applied, returns an empty sequence *)
val no : ('a, 'b) multifun

(** tries to apply tactics, returns previous state on failure *)
val attempt : ('a, 'a) multifun -> ('a, 'a) multifun

(** repeats the provided tactic until it fails *)
val repeat : ('a, 'a) multifun -> ('a, 'a) multifun

(** same as repeat, but deterministic, i.e. it considers only
    the first value returned by the provided tactic *)
val repeat_determ : ('a, 'a) multifun -> ('a, 'a) multifun

(** applies tactic until the state satisfies the predicate *)
val depth_first : ('a -> bool) -> ('a, 'a) multifun -> ('a, 'a) multifun

val depth_iter : ('a -> bool) -> int -> ('a, 'a) multifun -> ('a, 'a) multifun

(** applies first applicable tactic from the provided list *)
val first_fun : ('a -> ('b,'c) multifun) list -> 'a -> ('b,'c) multifun

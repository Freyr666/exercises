
type term = Var of string
          | Param of string * string list
          | Bound of int
          | Fun of string * term list

and formulae = Pred of string * term list
             | Rel of string * formulae list
             | Quant of string * string * formulae

and goal = formulae list * formulae list

(** Precedence of relation (needed for parsing) *)
val rel_precedence : string -> int

(** [abstract level term form] replaces all occurances
    of [term] in [form] by [Bound level] *)
val abstract : int -> term -> formulae -> formulae

(** [subst level term form] replaces all occurances
    of [Bound level] in [form] by [term] *)
val subst : int -> term -> formulae -> formulae

(** [term_vars vars term] appends all the variables
    occuring in [term] to the provided list [vars] *)
val term_vars : string list -> term -> string list

(** [goal_vars vars goal] appends all the variables
    occuring in [goal] to the provided list [vars] *)
val goal_vars : string list -> goal -> string list

(** [term_params params term] appends all the parameters
    occuring in [term] to the provided list [params] *)
val term_params : (string * string list) list
                  -> term
                  -> (string * string list) list

(** [goal_params params term] appends all the parameters
    occuring in [goal] to the provided list [params] *)
val goal_params : (string * string list) list
                  -> goal
                  -> (string * string list) list

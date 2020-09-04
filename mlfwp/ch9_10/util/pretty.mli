
type t

val blo : int -> t list -> t

val str : string -> t

val brk : int -> t

val show : out_channel -> t -> int -> unit

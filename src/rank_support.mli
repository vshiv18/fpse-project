type t

val create : Bitv.t -> t

(** Results may be off by as much as (log_2 n)/2 if given another bitvector *)
val rank1 : t -> Bitv.t -> int -> int

val rank0 : t -> Bitv.t -> int -> int

val overhead: t -> int

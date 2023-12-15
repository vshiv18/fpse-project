type t

val create : Rank_support.t -> t

(** Results are undefined if given a different bitvector *)
val select1 : t -> Bitv.t -> int -> int option

val select0 : t -> Bitv.t -> int -> int option

val overhead : t -> int

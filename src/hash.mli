module type S = sig
    val hash : string -> int
    val next_hash : string -> int -> int
    val is_trigger_string : string -> bool
end

(** [b] - base of rolling hash, [p] - prime used for mod **)
module type Params = sig
  val b : int
  val p : int
end

module Default : Params

module MakeRollHash (_ : Params) : S
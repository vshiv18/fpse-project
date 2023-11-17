(** Rolling hash returns the hash over a window size, implicitly assumed to be the length of the
    string given in hash; next and prev hash values are found by rolling the hash left/right **)
module type S = sig
  val hash : string -> int
  val next_hash : string -> int -> int
  val prev_hash : string -> int -> int
  (** Returns true if the [hash str] is 0 **)
  val is_trigger_string : string -> bool
end

(** [b] - base of rolling hash, [p] - prime used for mod **)
module type Params = sig
  val b : int
  val p : int
end

module Default : Params
module MakeRollHash (_ : Params) : S

(** Implements an FM-index over a BWT, which supports exact pattern matching queries. Requires some module which can support
    rank, access, and select queries over characters and defines the text; such as a wavelet tree. **)
module FM_index (_ : sig
    val rank : char -> int -> int
    val access : int -> char
    val select : char -> int -> int
  end) : 
  sig
    (** [count str] returns the number of occurrences of [str] within the text **)
    val count : string -> int
    (** [locate str] returns the locations (offsets) of occurrences of [str] within the text **)
    val locate : string -> int list
    (** [extract i m] returns the substring of length [m] starting from position [i] **)
    val extract : int -> int -> string
    (** [lf i] returns the position of the preceding character of BWT[i] wrt. the original text**)
    val lf : int -> int
    (** [lf i c] returns the position of the preceding character c of the first c before position i**)
    val lf_c : int -> char -> int
  end

(* The R_index implements the same pattern matching as an FM-index, but does so in runs-compressed space *)
module R_index (_ : sig
    val rank : char -> int -> int
    val access : int -> char
    val select : char -> int -> int
end) : 
    sig
    val count : string -> int
    val locate : string -> int list
    val extract : int -> int -> string
    val lf : int -> int
    val lf_c : int -> char -> int
end
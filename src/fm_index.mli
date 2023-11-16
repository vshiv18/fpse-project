module FM_index (_ : sig
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
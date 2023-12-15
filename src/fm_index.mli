(** Implements an FM-index over a BWT, which supports exact pattern matching queries. Requires some module which can support
    rank, access, and select queries over characters and defines the text; such as a wavelet tree. **)
    module FM_index :
    sig
      type t
      (** [construct str] returns the FM index object for [str]  **)
      val construct : string -> t
      val of_file : string -> t
      (** [count str] returns the number of occurrences of [str] within the text **)
      val exists : t -> string -> bool
      (** [count index str] returns the number of occurrences of [str] within the [index] **)
      val count : t -> string -> int option

      val lf_range : t -> int * int -> char -> int * int
      val lf : t -> int -> int

      val serialize : t -> string -> unit
      val deserialize : string -> t
    end
  
(* The R_index implements the same pattern matching as an FM-index, but does so in runs-compressed space *)
(* module R_index (_ : sig
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
end *)
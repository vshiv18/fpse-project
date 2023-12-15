module type S =
  sig
    type elt
    (** The type of the wavelet tree elements. *)

    type t
    (** The type of wavelet trees. *)

    val build: elt list -> t

    val access: t -> int -> elt
    (** Access a character at a given 1-indexed position
        @raise Invalid argument if index outside range *)

    val rank: t -> elt -> int -> int option
    (** Get the rank of a character at a given 1-indexed position
        Returns None if the character isn't in the alphabet
        @raise Invalid argument if index outside range *)

    val select: t -> elt -> int -> int option
    (** Get the (1-indexed) position of the nth occurrence of a character
        @raise Invalid argument if index outside range *)

    val alphabet_size: t -> int

    val cardinal: t -> int
    (** Length of the string indexed by the tree *)
  end

module Make (Ord : Set.OrderedType) : S with type elt = Ord.t  

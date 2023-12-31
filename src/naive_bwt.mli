(** A sequences holds a comparable item type and supports access (ideally constant time) and length. It also
    defines a null item, which is the lexicographically least item that terminates the sequence. Sequences
    can use mutatable data structures to implement, but Sequence itself does not support any mutation over them. **)
module type Sequence = sig
  module Item : sig
    type t [@@deriving compare, sexp, hash]
  end

  type t

  (** null should not be present anywhere in the least and should be of lexicographically least order **)
  val null : Item.t
  val get : t -> int -> Item.t
  val length : t -> int
  val of_list : Item.t list -> t
  val to_list : t -> Item.t list
  val of_seq : t -> t
  val fold : t -> init:'acc -> f:('acc -> Item.t -> 'acc) -> 'acc
end

module CharSequence : Sequence with type t = string and type Item.t = char
module IntSequence : Sequence with type t = Int.t Array.t and type Item.t = int

(** Text is built over a sequence, and is used to perform algorithms over it **)
module Text (Sequence : Sequence) : sig
  (* type t *)
  type text = Sequence.t

  (** [buildText txt] is used to format the sequence as expected, or simply cast it **)
  val buildText : text -> text
  (** [compare txt i j] compares the suffixes of [txt] starting at [i] and [j] respectively. 
      Should return some ordering of them, a naive example being character by character comparison. **)
  val compare : text -> int -> int -> int
  (** [getSA txt] returns a list where SA[i] is the ith lexicographically sorted suffix among all of them in [txt] **)
  val getSA : text -> int list

  (* val getSuffix : text -> int -> text *)
  (** Returns the Burrows-Wheeler Transform of a text; sort cyclic rotations as a matrix and take its last column **)
  val getBWT : text -> text
  val bwt_from_SA : text -> int list -> text
  (** Run-length encode a BWT, e.g. AAABB$AA returns [(A, 3); (B, 2); ($, 1); (A, 2)] **)
  val rle_BWT : text -> (Sequence.Item.t * int) list
end


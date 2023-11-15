module type Sequence = sig
  module Item : sig
    type t [@@deriving compare]
  end

  type t

  val null : Item.t
  val get : t -> int -> Item.t
  val length : t -> int
  val of_list : Item.t list -> t
  val of_seq : t -> t
end
module StringSequence : Sequence
module IntSequence : Sequence

module Text (Sequence : Sequence) : sig
  (* type t *)
  type text = Sequence.t

  val buildText : Sequence.t -> text
  val compare : text -> int -> int -> int
  val getSA : text -> int list
  (* val getSuffix : text -> int -> text *)
  val getBWT : text -> Sequence.t
end

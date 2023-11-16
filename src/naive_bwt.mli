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

module CharSequence : Sequence with type t = string
module IntSequence : Sequence with type t = Int.t Array.t

module Text (Sequence : Sequence) : sig
  (* type t *)
  type text = Sequence.t

  val buildText : text -> text
  val compare : text -> int -> int -> int
  val getSA : text -> int list

  (* val getSuffix : text -> int -> text *)
  val getBWT : text -> text
  val rle_BWT : text -> (Sequence.Item.t * int) list
end

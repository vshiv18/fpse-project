module type S = sig
  type t

  val write : string -> t -> unit
  val write_list : string -> t list -> unit
  val read : string -> t
  val read_list : string -> t list
end

module Int32Serializer : S with type t = int

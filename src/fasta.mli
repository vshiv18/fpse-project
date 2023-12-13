type 'a chunk = Stop of 'a | Continue of 'a

module Chunk : sig
  val value : 'a chunk -> 'a
end

module type S = sig
  type t = {
    channel : In_channel.t;
    buffer : Buffer.t;
    chunk_size : int;
    mutable char_in_sequence : bool;
  }

  val create : ?chunk_size:int -> string -> t
  val next : t -> string chunk
end

module FASTAStreamer : S

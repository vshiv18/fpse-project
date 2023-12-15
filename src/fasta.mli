type 'a chunk = { contents : 'a; is_last : bool }

module type Chunk = sig
  type t

  val ( + ) : t chunk -> t chunk -> t chunk
end

module StringChunk : Chunk with type t = string

module type S = sig
  type t = {
    channel : In_channel.t;
    buffer : Buffer.t;
    chunk_size : int;
    mutable char_in_sequence : bool;
  }

  val create : chunk_size:int -> string -> t
  val next : t -> string chunk
end

module FASTAStreamer : S

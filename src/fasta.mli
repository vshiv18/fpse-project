type 'a chunk = Stop of 'a | Continue of 'a

module Chunk : sig
  val value : 'a chunk -> 'a
end

module type FASTAStreamerConfig = sig
  val filename : string
  val chunk_size : int
end

module type S = sig
  val next : unit -> string chunk
end

module FASTAStreamer (_ : FASTAStreamerConfig) : S

open Core

type 'a chunk = { contents : 'a; is_last : bool }

module type Chunk = sig
  type t

  val ( + ) : t chunk -> t chunk -> t chunk
end

module StringChunk : Chunk with type t = string = struct
  type t = string

  let ( + ) (chunk1 : 'a chunk) (chunk2 : 'a chunk) : 'a chunk =
    {
      contents = String.concat [ chunk1.contents; chunk2.contents ];
      is_last = chunk2.is_last;
    }
end

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

module FASTAStreamer = struct
  type t = {
    channel : In_channel.t;
    buffer : Buffer.t;
    chunk_size : int;
    mutable char_in_sequence : bool;
  }

  type nucleotide = A | C | G | T

  let fasta_seq_separator : char = '>'
  let parse_seq_separator : char = '$'
  let new_line : char = '\n'

  let nucleotide_of_char (c : char) : nucleotide option =
    match c with
    | 'A' -> Some A
    | 'C' -> Some C
    | 'G' -> Some G
    | 'T' -> Some T
    | _ -> None

  let create ~(chunk_size : int) (filename : string) : t =
    {
      channel = In_channel.create filename;
      buffer = Buffer.create (Int.max 0 chunk_size);
      chunk_size;
      char_in_sequence = true;
    }

  let parse_char (streamer : t) (c : char) : bool =
    match streamer.char_in_sequence with
    | true -> (
        if Char.equal c parse_seq_separator then (
          streamer.char_in_sequence <- not streamer.char_in_sequence;
          true)
        else match nucleotide_of_char c with Some _ -> true | None -> false)
    | false ->
        if Char.equal c new_line then (
          streamer.char_in_sequence <- not streamer.char_in_sequence;
          false)
        else false

  let parse (streamer : t) : string =
    String.filter
      (if streamer.chunk_size = -1 then In_channel.input_all streamer.channel
       else
         Buffer.contents streamer.buffer
         |> String.tr ~target:fasta_seq_separator
              ~replacement:parse_seq_separator
         |> String.uppercase)
      ~f:(parse_char streamer)

  let next (streamer : t) : string chunk =
    if streamer.chunk_size = -1 then
      { contents = parse streamer; is_last = true }
    else (
      Buffer.reset streamer.buffer;
      match
        In_channel.input_buffer streamer.channel streamer.buffer
          ~len:streamer.chunk_size
      with
      | Some _ -> { contents = parse streamer; is_last = false }
      | None -> { contents = parse streamer; is_last = true })
end

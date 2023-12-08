open Core

type 'a chunk = Stop of 'a | Continue of 'a

module Chunk = struct
  let value (chunk : 'a chunk) : 'a =
    match chunk with Stop v | Continue v -> v
end

module type FASTAStreamerConfig = sig
  val filename : string
  val chunk_size : int
end

module type S = sig
  val next : unit -> string chunk
end

module FASTAStreamer (Config : FASTAStreamerConfig) : S = struct
  type nucleotide = A | C | G | T

  let fasta_seq_separator : char = '>'
  let parse_seq_separator : char = '$'
  let new_line : char = '\n'
  let char_in_sequence = ref true
  let toggle_char_in_sequence () = char_in_sequence := not !char_in_sequence

  let nucleotide_of_char (c : char) : nucleotide option =
    match c with
    | 'A' -> Some A
    | 'C' -> Some C
    | 'G' -> Some G
    | 'T' -> Some T
    | _ -> None

  let channel : In_channel.t = In_channel.create Config.filename
  let buffer : Buffer.t = Buffer.create Config.chunk_size

  let parse_char (c : char) : bool =
    match !char_in_sequence with
    | true -> (
        if Char.equal c parse_seq_separator then (
          toggle_char_in_sequence ();
          true)
        else match nucleotide_of_char c with Some _ -> true | None -> false)
    | false ->
        if Char.equal c new_line then (
          toggle_char_in_sequence ();
          false)
        else false

  let parse_buffer (buffer : Buffer.t) : string =
    String.filter
      (Buffer.contents buffer
      |> String.tr ~target:fasta_seq_separator ~replacement:parse_seq_separator
      |> String.uppercase)
      ~f:parse_char

  let next () : string chunk =
    Buffer.reset buffer;
    match In_channel.input_buffer channel buffer ~len:Config.chunk_size with
    | Some _ -> Continue (parse_buffer buffer)
    | None -> Stop (parse_buffer buffer)
end

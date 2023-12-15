open Core
open Bin_prot
open Write

module type S = sig
  type t

  val write : string -> t -> unit
  val write_list : string -> t list -> unit
  val read : string -> t
  val read_list : string -> t list
end

module Int32Serializer : S with type t = int = struct
  type t = int
  type writer = t Write.writer

  let writer : writer =
    bin_write_network32_int (* big-endian encoding for `input_binary_int`*)

  let buf_size = 4

  let write (filename : string) (x : t) : unit =
    let buf = Common.create_buf buf_size in
    let _ = writer buf ~pos:0 x in
    Out_channel.with_file filename ~f:(fun oc ->
        Out_channel.output_bytes oc (Bigstring.to_bytes buf))

  let write_list (filename : string) (ls : t list) : unit =
    let buf = Common.create_buf (buf_size * List.length ls) in
    let _ = List.fold ~init:0 ls ~f:(fun pos x -> writer buf ~pos x) in
    Out_channel.with_file filename ~f:(fun oc ->
        Out_channel.output_bytes oc (Bigstring.to_bytes buf))

  let read (filename : string) : t =
    In_channel.with_file filename ~f:(fun ic ->
        match In_channel.input_binary_int ic with
        | None -> failwith "empty file"
        | Some x -> x)

  let read_list (filename : string) : t list =
    In_channel.with_file filename ~f:(fun ic ->
        let rec f acc =
          match In_channel.input_binary_int ic with
          | None -> List.rev acc
          | Some x -> f (x :: acc)
        in
        f [])
end

module StringSerializer : S with type t = string = struct
  type t = string

  let separator : char = '\x01'

  let write (filename : string) (x : t) : unit =
    Out_channel.with_file filename ~f:(fun oc -> Out_channel.output_string oc x)

  let write_list (filename : string) (ls : t list) : unit =
    Out_channel.with_file filename ~f:(fun oc ->
        Out_channel.output_string oc
          (String.concat ~sep:(String.of_char separator) ls))

  let read (filename : string) : t = In_channel.read_all filename

  let read_list (filename : string) : t list =
    In_channel.read_all filename |> String.split ~on:separator
end

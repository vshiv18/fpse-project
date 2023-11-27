open Core

type t = { name : string; sequence : string } [@@deriving sexp]
type nucleotide = A | C | G | T [@@deriving sexp]

let nucleotide_of_char (c : char) : nucleotide option =
  match c with
  | 'A' -> Some A
  | 'C' -> Some C
  | 'G' -> Some G
  | 'T' -> Some T
  | _ -> None

let seq_separator = '>'

let clean_name (name : string) : string =
  String.fold_until name ~init:""
    ~f:(fun acc x ->
      if Char.is_whitespace x then Stop acc
      else Continue (acc ^ String.of_char x))
    ~finish:(fun acc -> acc)

let clean_sequence (sequence : string list) : string =
  String.concat ~sep:"" sequence
  |> String.uppercase
  |> String.filter ~f:(fun x ->
         match nucleotide_of_char x with Some _ -> true | None -> false)

let make_sequence (sequence : string) : t =
  match sequence with
  | "" -> failwith "Empty sequence"
  | _ -> (
      match String.split_lines sequence with
      | [] -> failwith "Invalid sequence"
      | _ :: [] -> failwith "Invalid sequence"
      | name :: sequence ->
          { name = clean_name name; sequence = clean_sequence sequence })

let get_sequences (content : string) : string list =
  String.split ~on:seq_separator content
  |> List.filter ~f:(fun s -> not (String.is_empty s))

let parse_fasta (filename : string) : t list =
  In_channel.read_all filename |> get_sequences |> List.map ~f:make_sequence

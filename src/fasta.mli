type t = { name : string; sequence : string }

val clean_name : string -> string
val clean_sequence : string list -> string
val make_sequence : string -> t
val get_sequences : string -> string list
val parse_fasta : string -> t list

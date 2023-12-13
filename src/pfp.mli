module type S = sig
  type text = string
  type dict
  type parse = string list * int list * int list

  val dict_to_alist : dict -> (string * int) list
  val initialize_streamer : ?chunk_size:int -> string -> Fasta.FASTAStreamer.t

  val trigger :
    string -> int -> Fasta.FASTAStreamer.t -> bool -> text * text * bool

  val hash : string -> int -> int list * dict
  val parse : string -> int -> parse
  val buildText : string -> text
  val parse_to_BWT : parse -> int -> string
  val getBWT : text -> int -> string
  val save_parse : parse -> string -> unit
  val load_parse : string -> parse
end

module PFP (_ : sig
  val is_trigger_string : string -> bool
end) : S

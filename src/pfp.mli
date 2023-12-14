module type S = sig
  type text = string
  type dict
  type parse = string list * int list * int list

  val dict_to_alist : dict -> (string * int) list
  val initialize_streamer : string -> chunk_size:int -> Fasta.FASTAStreamer.t

  val trigger :
    chunk:string ->
    phrase_start:int ->
    phrase_end:int ->
    window:int ->
    Fasta.FASTAStreamer.t ->
    bool ->
    text * int * int * text * bool

  val sorted_phrases : dict -> text list
  val hash : ?chunk_size:int -> string -> window:int -> int list * dict
  val parse : string -> int -> parse
  val buildText : string -> text

  (* Given the dictionary and parse of the BWT, use it to compute the BWT of the original text *)
  val parse_to_BWT : Out_channel.t -> parse -> int -> unit
  val getBWT : text -> int -> string
  val save_parse : parse -> string -> unit
  val load_parse : string -> parse
end

module PFP (_ : sig
  val is_trigger_string : string -> bool
end) : S

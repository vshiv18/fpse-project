(** Prefix Free Parsing (PFP) is defined on a string, and supports parsing into a dictionary of keywords and a parse which
    denoting those keywords replacing the original string. No keyword phrase is a proper prefix of another, and this property
    allows efficient BWT (see naive_bwt.ml) constrction. **)
module type PFP_S = sig
  type text = string

  (* type dict = string list *)
  (* module Dict = Map.Make(String) *)
  (** Dictionary holding keywords and their counts in the parse **)
  type dict
  type parse = string list * int list * int list

  (** [parse txt w] Returns the dictionary and the parse for [txt] where [w] is the windows size used to perform
      hash computation that decides how to parse the input. **)
  val parse : ?verbose:bool -> text -> int -> parse
  val buildText : string -> text
  (** Sanity check to analyse dictionary contents **)
  val dict_to_alist : dict -> (string * int) list
  (** Given the dictionary and parse of the BWT, use it to compute the BWT of the original text **)
  val parse_to_BWT : parse -> int -> string
  val getBWT : text -> int -> string
end

(** Requires a hash module which defines trigger strings which break keywords **)
module PFP (_ : sig
  val is_trigger_string : string -> bool
end) : PFP_S

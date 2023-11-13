module type PFP_S = 
  sig
  type text = string
  (* type dict = string list *)
  (* module Dict = Map.Make(String) *)
  type dict
  val parse: text -> int -> dict * (int list)
  val buildText: string -> text
  val dict_to_alist: dict -> (string * int) list
  end

module PFP (_ :sig val is_trigger: string -> bool end) : PFP_S


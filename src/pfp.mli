module type PFP_S = 
  sig
  type text = string
  type dict = string list
  val parse: text -> int -> dict * (int list)
  val buildText: string -> text
  val buildDict: dict -> string list
  end

module PFP (_ :sig val is_trigger: int -> string -> bool end) : PFP_S


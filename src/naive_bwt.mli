module Text : sig
  (* type t *)
  type text
  val setText: string -> text
  val compare: text -> int -> int -> int
  val getSA: text -> int list 
  val getSuffix: text -> int -> string
  val getBWT: text -> string 
end
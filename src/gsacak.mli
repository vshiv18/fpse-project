module type GSACAK = sig
  val getSA : string -> int Array.t
  val getSA_int : int Array.t -> int Array.t
  val getBWT : string -> string
end

module GSACAK : GSACAK
open Core

module type GSACAK = sig
  val getSA : string -> int Array.t
  val getSA_int : int Array.t -> int Array.t
  val getBWT : string -> string
end

module GSACAK = struct
  external sacak_c : string -> int Array.t option = "sacak_ocaml"

  let getSA (text : string) : int Array.t =
    match sacak_c text with Some x -> x | None -> [] |> List.to_array

  external sacak_int_c : int Array.t -> int ->  int Array.t option = "sacak_int_ocaml"

  let getSA_int (text : int Array.t) : int Array.t =
    let sigma = text |> Array.to_list |> Hash_set.of_list (module Int) |> Hash_set.length in
    match sacak_int_c text sigma with Some x -> x | None -> [] |> List.to_array

  let getBWT (text : string) : string =
    text |> getSA
    |> Array.map ~f:(fun idx ->
           if idx = 0 then '$' else String.get text (idx - 1))
    |> String.of_array
end

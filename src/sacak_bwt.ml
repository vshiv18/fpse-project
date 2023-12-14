open BigBWT.Gsacak
open Core

let () =
  let target_file =
    let argv = Sys.get_argv () |> Array.to_list in
    if List.length argv <> 2 then failwith "filename required"
    else 
    match argv with
    | _ :: file :: [] -> file
    | _ -> failwith "filename required"
  in
  let seq =
    In_channel.with_file target_file ~f:(fun channel ->
        In_channel.input_all channel)
  in
  let sa = (GSACAK.getSA seq) |> Array.to_list in
  let bwt = sa 
  |> List.map ~f:(fun idx ->
    if idx = 0 then '$' else String.get seq (idx - 1))
|> String.of_char_list in
  printf "%s%!" bwt
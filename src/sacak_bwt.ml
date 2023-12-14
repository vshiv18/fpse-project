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
  let bwt = GSACAK.getBWT seq in
  printf "%s%!" bwt
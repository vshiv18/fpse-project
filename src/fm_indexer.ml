open Core
module Parser = BigBWT.Pfp.PFP (RollHash.Hash.DefaultHasher)
open Serialize
open Fm_index

type do_mode = Build of string option | Query of string option * string option

let save_parse parse out_prefix = 
  let dict, freq, parse = parse in
    StringSerializer.write_list (out_prefix ^ ".dict") dict;
    Int32Serializer.write_list (out_prefix ^ ".occs") freq;
    Int32Serializer.write_list (out_prefix ^ ".parse") parse

let build input_fname out_prefix =
  let window = 10 in
  let seq = In_channel.read_all input_fname in
  printf "Read input sequence from: %s\n%!" input_fname;
  let parse = Parser.parse seq window in
  printf "Generated parse!%!";
  save_parse parse out_prefix;
  printf "Saved parse to %s*\n%!" out_prefix;
  let outfname = Out_channel.create (out_prefix ^ ".bwt") in
  Parser.parse_to_BWT outfname parse window;
  printf "BWT computed!\n%!"; 
  let _ = FM_index.construct_from_file (out_prefix ^ ".bwt") in
  (* serialize fmi in *)
  printf "FM Index built!\n%!" 

let query _ _ = ()

let run mode out_prefix = 
  match mode with 
  | Build input_fname -> build (Option.value_exn input_fname) out_prefix
  | Query (index, pattern_fname) -> query (Option.value_exn index) (Option.value_exn pattern_fname)




let command =
  Command.basic ~summary:"OCaml FM index"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command 
    mode = anon ("mode" %: string)
    and input_fname =
       flag "-i" (optional string)
         ~doc:"string path to file to index."
    and pattern_fname = 
       flag "-p" (optional string)
         ~doc:"string path to file for pattern."
    and out_prefix =
    flag "--out-prefix"
      (optional_with_default "./index" string)
      ~doc:"string Path to store parse results."
     in
     let mode = match mode with | "build" -> Build input_fname | "run" -> Query (input_fname, pattern_fname) | _ -> failwith "[build|run] mode required!" in
    fun () -> run mode out_prefix)

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command

(* 
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
  printf "%s%!" bwt *)
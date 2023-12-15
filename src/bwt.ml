open Core
module Parser = BigBWT.Pfp.PFP (RollHash.Hash.DefaultHasher)

type do_mode = Make of string | Load of string | None of string

let get_do_mode (input_fname : string option) (parse_dir : string option) =
  match (input_fname, parse_dir) with
  | Some target, None -> Make target
  | Some _, Some parse_dir ->
      printf "Overwriting parse_dir in %s\n%!" parse_dir;
      Load parse_dir
  | None, Some parse_dir -> Load parse_dir
  | _ -> None "Exactly one of -i input_file or --parse_dir needs to be set"

let prepare_parse_dir (input_fname : string) (out_dir : string) : string =
  let target_name, _ =
    let _, fname = Filename.split input_fname in
    Filename.split_extension fname
  in
  let parse_dir = Filename.concat out_dir target_name in
  match Sys_unix.file_exists parse_dir with
  | `Yes -> parse_dir
  | `No | `Unknown ->
      Core_unix.mkdir parse_dir;
      parse_dir

let do_parse_bwt (input_fname : string) (chunk_size : int) (window : int)
    (out_dir : string) : unit =
  let parse_dir = prepare_parse_dir input_fname out_dir in
  printf "Read input sequence from: %s\n%!" input_fname;
  let parse = Parser.parse ~chunk_size input_fname ~window in
  printf "Generated parse!\n%!";
  Parser.save_parse parse parse_dir;
  printf "Saved parse to %s\n%!" parse_dir;
  let outfname = Out_channel.create (Filename.concat parse_dir "bwt") in
  Parser.parse_to_BWT outfname parse window;
  printf "BWT computed!\n%!"

let do_bwt (parse_dir : string) (window : int) : unit =
  let parse = Parser.load_parse parse_dir in
  printf "Loaded parse from %s\n%!" parse_dir;
  let outfname = Out_channel.create (Filename.concat parse_dir "bwt") in
  Parser.parse_to_BWT outfname parse window;
  printf "BWT computed!\n%!"

let do_run (input_fname : string option) (chunk_size : int)
    (parse_dir : string option) (window : int) (out_dir : string) : unit =
  match get_do_mode input_fname parse_dir with
  | Make target -> do_parse_bwt target chunk_size window out_dir
  | Load parse_dir -> do_bwt parse_dir window
  | None msg -> printf "Invalid arguments: %s\n%!" msg

let command =
  Command.basic ~summary:"OCaml BigBWT"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command input_fname =
       flag "-i" (optional string)
         ~doc:"string Path to file to compute parse of."
     and chunk_size =
       flag "--chunk-size"
         (optional_with_default (-1) int)
         ~doc:"int Chunk size."
     and parse_dir =
       flag "--from-parse" (optional string)
         ~doc:"string Path to directory to load parse from."
     and window =
       flag "--window" (optional_with_default 10 int) ~doc:"int Window size."
     and out_dir =
       flag "--out-dir"
         (optional_with_default "./out" string)
         ~doc:"string Path to store parse results."
     in
     fun () -> do_run input_fname chunk_size parse_dir window out_dir)

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command

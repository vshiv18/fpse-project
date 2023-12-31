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

let prepare_parse_dir (out_dir : string) : string =
  let parse_dir = out_dir in
  match Sys_unix.file_exists parse_dir with
  | `Yes -> parse_dir
  | `No | `Unknown ->
      Core_unix.mkdir parse_dir;
      parse_dir

let do_parse_bwt (target : string) (window : int) (out_dir : string) : unit =
  let parse_dir = prepare_parse_dir out_dir in
  printf "Read input sequence from: %s\n%!" target;
  let seq = In_channel.read_all target in
  let parse = Parser.parse seq window in
  printf "Generated parse!%!";
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

let do_run (input_fname : string option) (parse_dir : string option)
    (window : int) (out_dir : string) : unit =
  match get_do_mode input_fname parse_dir with
  | Make target -> do_parse_bwt target window out_dir
  | Load parse_dir -> do_bwt parse_dir window
  | None msg -> printf "Invalid arguments: %s\n%!" msg

let command =
  Command.basic ~summary:"OCaml BigBWT"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command input_fname =
       flag "-i" (optional string) ~doc:"string Path to file to compute BWT of."
     and parse_dir =
       flag "--from-parse" (optional string)
         ~doc:"string Path to directory to load parse from."
     and window =
       flag "--window" (optional_with_default 10 int) ~doc:"int Window size."
     and out_dir =
       flag "--out-dir"
         (optional_with_default "./" string)
         ~doc:"string Path to store parse results."
     in
     fun () -> do_run input_fname parse_dir window out_dir)

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command

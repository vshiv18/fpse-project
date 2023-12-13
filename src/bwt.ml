open Core
module Parser = BigBWT.Pfp.PFP (RollHash.Hash.DefaultHasher)

type do_mode = Make of string | Load of string | None of string

let get_do_mode (filename : string option) (parse_dir : string option) =
  match (filename, parse_dir) with
  | Some filename, None -> Make filename
  | None, Some parse_dir -> Load parse_dir
  | _ -> None "Exactly one of `filename` or --from_parse has to be set"

let prepare_parse_dir (filename : string) (out_dir : string) : string =
  let target_name, _ = Filename.split_extension filename in
  let parse_dir = Filename.concat out_dir target_name in
  match Sys_unix.file_exists parse_dir with
  | `Yes -> parse_dir
  | `No | `Unknown ->
      Core_unix.mkdir parse_dir;
      parse_dir

let do_bwt (parse : Parser.parse) (parse_dir : string) (window : int) : unit =
  let bwt = Parser.parse_to_BWT parse window in
  printf "BWT computed (showing first 100 characters)\n%s...\n"
    (if String.length bwt > 100 then String.slice bwt 0 100 else bwt);
  Out_channel.write_all (Filename.concat parse_dir "bwt") ~data:bwt

let do_make (filename : string) (window : int) (out_dir : string) : unit =
  let parse_dir = prepare_parse_dir filename out_dir in
  let parse = Parser.parse filename window in
  printf "Generated parse!";
  Parser.save_parse parse parse_dir;
  printf "Saved parse to %s\n" parse_dir;
  do_bwt parse parse_dir window

let do_load (parse_dir : string) (window : int) : unit =
  let parse = Parser.load_parse parse_dir in
  printf "Loaded parse from %s\n" parse_dir;
  do_bwt parse parse_dir window

let do_run (filename : string option) (parse_dir : string option) (window : int)
    (out_dir : string) : unit =
  match get_do_mode filename parse_dir with
  | Make filename -> do_make filename window out_dir
  | Load parse_dir -> do_load parse_dir window
  | None msg -> printf "Invalid arguments: %s\n" msg

let command =
  Command.basic ~summary:"OCaml BigBWT"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command filename = anon (maybe ("filename" %: string))
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
     fun () -> do_run filename parse_dir window out_dir)

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command

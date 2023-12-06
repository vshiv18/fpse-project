open Core
module Parser = BigBWT.Pfp.PFP (RollHash.Hash.DefaultHasher)

type do_mode = Parse of string | BWT of string

let get_do_mode (parse_target : string option) (parse_dir : string option) =
  match (parse_target, parse_dir) with
  | Some target, None -> Parse target
  | None, Some parse_dir -> BWT parse_dir
  | _ -> failwith "Exactly one of --parse or --parse_dir needs to be set"

let do_parse (target : string) (window : int) (out_dir : string) : unit =
  let target_name, _ = Filename.split_extension target in
  let seq = In_channel.read_all target in
  printf "Read input sequence from: %s\n" target;
  let parse = Parser.parse seq window in
  printf "Generated parse!";
  Parser.save_parse parse (Filename.concat out_dir target_name)

let do_bwt (parse_dir : string) (window : int) : unit =
  let parse = Parser.load_parse parse_dir in
  printf "Loaded parse from %s\n" parse_dir;
  let bwt = Parser.parse_to_BWT parse window in
  printf "BWT computed (showing first 100 characters)\n%s...\n"
    (String.slice bwt 0 100);
  Out_channel.write_all (Filename.concat parse_dir "bwt") ~data:bwt

let do_run (parse_target : string option) (parse_dir : string option)
    (window : int) (out_dir : string) : unit =
  match get_do_mode parse_target parse_dir with
  | Parse target -> do_parse target window out_dir
  | BWT parse_dir -> do_bwt parse_dir window

let command =
  Command.basic ~summary:"OCaml BigBWT"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command parse_target =
       flag "--parse" (optional string) ~doc:"Path to file to compute parse of."
     and parse_dir =
       flag "--bwt" (optional string)
         ~doc:"Path to parse directory to load parse from."
     and window =
       flag "--window" (optional_with_default 10 int) ~doc:"Window size."
     and out_dir =
       flag "--out_dir"
         (optional_with_default "out" string)
         ~doc:"Path to store parse results"
     in
     fun () -> do_run parse_target parse_dir window out_dir)

let () = Command_unix.run ~version:"0.1" ~build_info:"RWO" command

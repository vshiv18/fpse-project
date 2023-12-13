open Core
module Parser = BigBWT.Pfp.PFP (RollHash.Hash.DefaultHasher)

type do_mode = Parse of string | BWT of string | None of string

let get_do_mode (input_fname : string option) (parse_dir : string option) =
  match (input_fname, parse_dir) with
  | Some target, None -> Parse target
  | Some _, Some parse_dir -> printf "Overwriting parse_dir in %s\n" parse_dir; BWT parse_dir
  | _ -> None "Exactly one of -i input_file or --parse_dir needs to be set"

let prepare_parse_dir (out_dir : string) : string =
  (* let target_name, _ = let _, fname = Filename.split target in Filename.split_extension fname in *)
  let parse_dir = out_dir in
  (* let parse_dir = Filename.concat out_dir target_name in *)
  match Sys_unix.file_exists parse_dir with
  | `Yes -> parse_dir
  | `No | `Unknown ->
      Core_unix.mkdir parse_dir;
      parse_dir

let do_parse_bwt (target : string) (window : int) (out_dir : string) : unit =
  let parse_dir = prepare_parse_dir out_dir in
  let seq = In_channel.read_all target in
  printf "Read input sequence from: %s\n" target;
  let parse = Parser.parse seq window in
  printf "Generated parse!";
  let bwt, sa = Parser.parse_to_BWT parse window in
  Parser.save_parse parse parse_dir;
  printf "Saved parse to %s\n" parse_dir;
  printf "BWT computed (showing first 100 characters)\n%s...\n"
    (String.slice bwt 0 100);
  printf "SA computed (showing first 100 / %d entries)\n%s...\n" (List.length sa)
    ((List.take sa 100) |> List.map ~f:Int.to_string |> String.concat ~sep:", ");
  printf "%d\n" (List.length sa);
  Out_channel.write_all (Filename.concat parse_dir "bwt") ~data:bwt;
  Out_channel.write_all (Filename.concat parse_dir "sa") ~data:(sa |> List.to_string ~f:Int.to_string)


let do_bwt (parse_dir : string) (window : int) : unit =
  let parse = Parser.load_parse parse_dir in
  printf "Loaded parse from %s\n" parse_dir;
  let bwt, sa = Parser.parse_to_BWT parse window in
  printf "BWT computed (showing first 100 characters)\n%s...\n"
    (String.slice bwt 0 100);
  printf "SA computed (showing first 100 / %d entries)\n%s...\n" (List.length sa)
    ((List.take sa 100) |> List.map ~f:Int.to_string |> String.concat ~sep:", ");
  printf "%d\n" (List.length sa);
  Out_channel.write_all (Filename.concat parse_dir "bwt") ~data:bwt;
  Out_channel.write_all (Filename.concat parse_dir "sa") ~data:(sa |> List.to_string ~f:Int.to_string)

let do_run (input_fname : string option) (parse_dir : string option)
    (window : int) (out_dir : string) : unit =
  match get_do_mode input_fname parse_dir with
  | Parse target -> do_parse_bwt target window out_dir
  | BWT parse_dir -> do_bwt parse_dir window
  | None msg -> printf "Invalid arguments: %s\n" msg

let command =
  Command.basic ~summary:"OCaml BigBWT"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command input_fname =
       flag "-i" (required string)
         ~doc:"string Path to file to compute parse of."
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
     fun () -> do_run (Some input_fname) parse_dir window out_dir)

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command

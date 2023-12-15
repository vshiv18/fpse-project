open BigBWT.Gsacak
open Core

let prepare_parse_dir (out_dir : string) : string =
  (* let target_name, _ = let _, fname = Filename.split target in Filename.split_extension fname in *)
  let parse_dir = out_dir in
  (* let parse_dir = Filename.concat out_dir target_name in *)
  match Sys_unix.file_exists parse_dir with
  | `Yes -> parse_dir
  | `No | `Unknown ->
      Core_unix.mkdir parse_dir;
      parse_dir

let do_run (input_fname : string) (out_dir : string) : unit =
  let parse_dir = prepare_parse_dir out_dir in
  let seq =
    In_channel.with_file input_fname ~f:(fun channel ->
        In_channel.input_all channel)
  in
  let bwt = GSACAK.getBWT seq in
  Out_channel.output_string
    (Out_channel.create (Filename.concat parse_dir "bwt"))
    bwt

let command =
  Command.basic ~summary:"SACAK BWT"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command input_fname =
       flag "-i" (required string) ~doc:"string Path to file to compute BWT of."
     and out_dir =
       flag "--out-dir" (required string) ~doc:"string Path to store output in."
     in
     fun () -> do_run input_fname out_dir)

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command

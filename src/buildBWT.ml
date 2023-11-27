open BigBWT
open Core

module Parser = BigBWT.Pfp.PFP(struct
    let is_trigger s = List.mem [ "AAC"; "ACG"; "TAG"; "TTA";"TGA";"GTT"] s ~equal:String.( = )
end)
module ParseMapper = Map.Make (String)
module InvList = Map.Make (String)
module IntBWT = Naive_bwt.Text (Naive_bwt.IntSequence)

let newfilename fname = match Filename.split_extension fname with
  | prefix, Some exn -> prefix ^ "_bwt." ^ exn
  | prefix, None -> prefix ^ "_bwt.fa"

let () =
  let w = 3 in
  let target_file = 
    match Sys.get_argv () |> Array.to_list with
    | _ :: file :: _ -> file
    | _  -> failwith "filename required"
  in
  let seq = In_channel.with_file target_file ~f:(fun channel -> In_channel.input_all channel) in
  let bwt = Parser.getBWT seq w in
  let outfile = newfilename target_file in
  Out_channel.write_all outfile ~data:bwt
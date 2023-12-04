open BigBWT
open Core
open RollHash

(* module Parser = BigBWT.Pfp.PFP(struct
       let is_trigger_string s = List.mem [ "AAC"; "ACG"; "TAG"; "TTA";"TGA";"GTT"] s ~equal:String.( = )
   end) *)

module Hasher = Hash.MakeRollHash (struct
  let b = 256
  let p = 27162335252586509
end)

module Parser = BigBWT.Pfp.PFP(Hasher)

module ParseMapper = Map.Make (String)
module InvList = Map.Make (String)
module IntBWT = Naive_bwt.Text (Naive_bwt.IntSequence)

let newfilename fname =
  match Filename.split_extension fname with
  | prefix, Some exn -> prefix ^ "_bwt." ^ exn
  | prefix, None -> prefix ^ "_bwt.fa"

let () =
  let w = 10 in
  let target_file =
    match Sys.get_argv () |> Array.to_list with
    | _ :: file :: _ -> file
    | _ -> failwith "filename required"
  in
  let seq =
    In_channel.with_file target_file ~f:(fun channel ->
        In_channel.input_all channel)
  in
  let () = printf "Read input sequence\n%!" in
  let dict, freq, parse = Parser.parse seq w in
  let () = printf "Generated parse\n%!" in
  let bwt = Parser.parse_to_BWT (dict, freq, parse) w in
  let () = printf "BWT computed: %s...\n%!" (String.slice bwt 0 100) in
  let outfile = newfilename target_file in
  Out_channel.write_all outfile ~data:bwt

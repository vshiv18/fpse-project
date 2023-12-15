open Core
open OUnit2
open BigBWT.Pfp

module TestHash = struct
  let is_trigger_string s =
    List.mem [ "AC"; "AG"; "TG"; "$$" ] s ~equal:String.( = )
end

let filename = "./data/parse.test"

module Parser = PFP (TestHash)

let test_correctness _ =
  let parse = Parser.parse filename ~window:2 in
  assert_equal [ "$GATTAC"; "ACATG"; "AGATA$$"; "TGGATAC"; "TGGATTAG" ]
  @@ parse.phrases;
  assert_equal [ 1; 2; 1; 1; 1 ] @@ parse.freqs;
  assert_equal [ 0; 1; 3; 1; 4; 2 ] @@ parse.parse

(* let test_hash _ =
     let gt_hash =
       List.map
         ~f:(fun p -> Hashtbl.hash p)
         [ "$GATTAC"; "ACATG"; "TGGATAC"; "ACATG"; "TGGATTAG"; "AGATA$$" ]
     in
     let gt_counts =
       [
         ("$GATTAC", 1);
         ("ACATG", 2);
         ("AGATA$$", 1);
         ("TGGATAC", 1);
         ("TGGATTAG", 1);
       ]
     in
     let hash, dict_count = Parser.hash filename ~window:2 in
     assert_equal gt_hash @@ hash;
     assert_equal gt_counts @@ Parser.dict_to_alist dict_count;
     let hash, dict_count = Parser.hash ~chunk_size:7 filename ~window:2 in
     assert_equal gt_hash @@ hash;
     assert_equal gt_counts @@ Parser.dict_to_alist dict_count

   let test_trigger _ =
     let streamer = Parser.initialize_streamer ~chunk_size:5 filename in
     let chunk, is_last_chunk =
       (Fasta.Chunk.value (Fasta.FASTAStreamer.next streamer), false)
     in
     List.fold
       ~init:(String.slice chunk 1 0, 0, 0, is_last_chunk)
       ~f:(fun (chunk, phrase_start, phrase_end, is_last_chunk) t ->
         let t', phrase_start, phrase_end, chunk, is_last_chunk =
           Parser.trigger ~chunk ~phrase_start ~phrase_end ~window:2 streamer
             is_last_chunk
         in
         assert_equal t @@ t';
         (chunk, phrase_start, phrase_end + 1, is_last_chunk))
       [
         "GA";
         "AT";
         "TT";
         "TA";
         "AC";
         "CA";
         "AT";
         "TG";
         "GG";
         "GA";
         "AT";
         "TA";
         "AC";
         "CA";
         "AT";
         "TG";
         "GG";
         "GA";
         "AT";
         "TT";
         "TA";
         "AG";
         "GA";
         "AT";
         "TA";
         "A$";
         "$$";
       ]
     |> ignore *)

let pfp_tests =
  "PFP tests"
  >::: [
         "test correctness" >:: test_correctness;
         (* "test hash" >:: test_hash; *)
         (* "test trigger" >:: test_trigger; *)
       ]

let series = "PFP tests" >::: [ pfp_tests ]
let () = run_test_tt_main series

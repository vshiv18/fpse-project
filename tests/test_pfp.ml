open Core
open OUnit2
open BigBWT.Pfp

module TestHash = struct
  let is_trigger_string s =
    List.mem [ "AC"; "AG"; "TG"; "$$" ] s ~equal:String.( = )
end

let filename = "./parse.fa"

module Parser = PFP (TestHash)

let test_correctness _ =
  let phrases, freqs, parse = Parser.parse filename 2 in
  assert_equal [ "$GATTAC"; "ACATG"; "AGATA$$"; "TGGATAC"; "TGGATTAG" ]
  @@ phrases;
  assert_equal [ 1; 2; 1; 1; 1 ] @@ freqs;
  assert_equal [ 0; 1; 3; 1; 4; 2 ] @@ parse

let test_hash _ =
  let hash, dict_count = Parser.hash filename 2 in
  assert_equal
    (List.map
       ~f:(fun p -> Hashtbl.hash p)
       [ "$GATTAC"; "ACATG"; "TGGATAC"; "ACATG"; "TGGATTAG"; "AGATA$$" ])
  @@ hash;
  assert_equal
    [
      ("$GATTAC", 1);
      ("ACATG", 2);
      ("AGATA$$", 1);
      ("TGGATAC", 1);
      ("TGGATTAG", 1);
    ]
  @@ Parser.dict_to_alist dict_count

let test_trigger _ =
  let streamer = Parser.initialize_streamer ~chunk_size:20 filename in
  let chunk, is_last_chunk =
    (Fasta.Chunk.value (Fasta.FASTAStreamer.next streamer), false)
  in
  List.fold
    ~init:(String.drop_prefix chunk 1, is_last_chunk)
    ~f:(fun (chunk, is_last_chunk) t ->
      let t', chunk, is_last_chunk =
        Parser.trigger chunk 2 streamer is_last_chunk
      in
      assert_equal t @@ t';
      (String.drop_prefix chunk 1, is_last_chunk))
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
  |> ignore

let pfp_tests =
  "PFP tests"
  >::: [
         "test correctness" >:: test_correctness;
         "test hash" >:: test_hash;
         "test trigger" >:: test_trigger;
       ]

let series = "PFP tests" >::: [ pfp_tests ]
let () = run_test_tt_main series

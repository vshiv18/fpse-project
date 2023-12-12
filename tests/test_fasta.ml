(* open Core *)
open OUnit2
open Fasta

let test_small_chunk _ =
  let module Config : FASTAStreamerConfig = struct
    let filename = "./fasta.test"
    let chunk_size = 10
  end in
  let module Streamer = FASTAStreamer (Config) in
  assert_equal (Continue "$") @@ Streamer.next ();
  assert_equal (Continue "") @@ Streamer.next ();
  assert_equal (Continue "ACTG") @@ Streamer.next ()

let test_large_chunk _ =
  let module Config : FASTAStreamerConfig = struct
    let filename = "./fasta.test"
    let chunk_size = 200
  end in
  let module Streamer = FASTAStreamer (Config) in
  assert_equal (Stop "$ACTGACTGACTGACTG$ACTGACTGACTGACTG") @@ Streamer.next ()

let parse_fasta_tests =
  "parse_fasta tests"
  >: test_list
       [
         "test small chunk" >:: test_small_chunk;
         "test large chunk" >:: test_large_chunk;
       ]

let series = "FASTA tests" >::: [ parse_fasta_tests ]
let () = run_test_tt_main series

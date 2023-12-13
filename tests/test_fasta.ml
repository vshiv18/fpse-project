open Core
open OUnit2
open Fasta

let filename = "./fasta.fa"

let test_small_chunk _ =
  let chunk_size = 10 in
  let streamer = FASTAStreamer.create ~chunk_size filename in
  assert_equal (Continue "$") @@ FASTAStreamer.next streamer;
  assert_equal (Continue "") @@ FASTAStreamer.next streamer;
  assert_equal (Continue "ACTG") @@ FASTAStreamer.next streamer

let test_large_chunk _ =
  let chunk_size = 200 in
  let streamer = FASTAStreamer.create ~chunk_size filename in
  assert_equal (Stop "$ACTGACTGACTGACTG$ACTGACTGACTGACTG")
  @@ FASTAStreamer.next streamer

let fastastreamer_tests =
  "FASTAStreamer tests"
  >: test_list
       [
         "test small chunk size" >:: test_small_chunk;
         "test large chunk size" >:: test_large_chunk;
       ]

let series = "FASTAStreamer tests" >::: [ fastastreamer_tests ]
let () = run_test_tt_main series

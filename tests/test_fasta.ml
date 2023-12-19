open Core
open OUnit2
open Fasta

let filename = "./data/fasta.test"

let test_small_chunk _ =
  let chunk_size = 10 in
  let streamer = FASTAStreamer.create ~chunk_size filename in
  assert_equal { contents = "$"; is_last = false }
  @@ FASTAStreamer.next streamer;
  assert_equal { contents = ""; is_last = false } @@ FASTAStreamer.next streamer;
  assert_equal { contents = "ACTG"; is_last = false }
  @@ FASTAStreamer.next streamer

let test_large_chunk _ =
  let chunk_size = 200 in
  let streamer = FASTAStreamer.create ~chunk_size filename in
  assert_equal
    { contents = "$ACTGACTGACTGACTG$ACTGACTGACTGACTG"; is_last = true }
  @@ FASTAStreamer.next streamer

let test_all_chunk _ =
  let chunk_size = -1 in
  let streamer = FASTAStreamer.create ~chunk_size filename in
  assert_equal
    { contents = "$ACTGACTGACTGACTG$ACTGACTGACTGACTG"; is_last = true }
  @@ FASTAStreamer.next streamer

let fastastreamer_tests =
  "FASTAStreamer tests"
  >: test_list
       [
         "test small chunk size" >:: test_small_chunk;
         "test large chunk size" >:: test_large_chunk;
         "test all chunk size" >:: test_all_chunk;
       ]

let series = "FASTAStreamer tests" >::: [ fastastreamer_tests ]
let () = run_test_tt_main series

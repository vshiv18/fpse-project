open Core
open OUnit2
open Fm_index

let text = "abbabbabbc"

module SaisBWT = BigBWT.Sais.SAIS

let test_fm_index _ =
  let fmi = FM_index.construct (SaisBWT.getBWT text) in
  let occ count = match count with | Some x -> x | None -> 0 in
  assert_equal 3 @@ occ (FM_index.count fmi "abb");
  assert_equal 2 @@ occ (FM_index.count fmi "abba");
  assert_equal 0 @@ occ (FM_index.count fmi "abbd");
  assert_equal 6 @@ occ (FM_index.count fmi "b");
  assert_equal true @@ FM_index.exists fmi "abb"

let fm_index_tests =
  "fm_index_tests"
  >::: [
         "test_fm_index" >:: test_fm_index;
       ]

let series = "Project Tests" >::: [ fm_index_tests ]
let () = run_test_tt_main series

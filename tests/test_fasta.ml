open Core
open OUnit2
open Fasta

let dummy_fasta =
  ">Sequence1 - metadata\nACTGXM*\n>Sequence2 - metadata\nACTGXM\nACTGXM*"

let test_get_sequences _ =
  assert_equal
    [
      "Sequence1 - metadata\nACTGXM*\n"; "Sequence2 - metadata\nACTGXM\nACTGXM*";
    ]
  @@ get_sequences dummy_fasta

let test_clean_name _ =
  assert_equal "Sequence1" @@ clean_name "Sequence1 - metadata";
  assert_equal "Sequence2" @@ clean_name "Sequence2 - metadata"

let test_clean_sequence _ =
  assert_equal "ACTG" @@ clean_sequence [ "ACTGXM*" ];
  assert_equal "ACTGACTG" @@ clean_sequence [ "ACTGXM"; "ACTGXM*" ]

let test_make_sequence _ =
  assert_raises (Failure "Empty sequence") (fun () -> make_sequence "");
  assert_raises (Failure "Invalid sequence") (fun () -> make_sequence "\n");
  assert_raises (Failure "Invalid sequence") (fun () ->
      make_sequence "Sequence1\n");
  assert_equal { name = "Sequence1"; sequence = "ACTG" }
  @@ make_sequence "Sequence1 - metadata\nACTGXM*\n";
  assert_equal { name = "Sequence2"; sequence = "ACTGACTG" }
  @@ make_sequence "Sequence2 - metadata\nACTGXM\nACTGXM*"

let test_parse_fasta _ =
  assert_equal
    [
      { name = "Sequence1"; sequence = "ACTG" };
      { name = "Sequence2"; sequence = "ACTGACTG" };
    ]
  @@ (get_sequences dummy_fasta |> List.map ~f:make_sequence)

let parse_fasta_tests =
  "parse_fasta tests"
  >: test_list
       [
         "test get_sequences" >:: test_get_sequences;
         "test clea_name" >:: test_clean_name;
         "test clean_sequence" >:: test_clean_sequence;
         "test make_sequence" >:: test_make_sequence;
         "test parse_fasta" >:: test_parse_fasta;
       ]

let series = "FASTA tests" >::: [ parse_fasta_tests ]
let () = run_test_tt_main series

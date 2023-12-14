open Core
open OUnit2
open BigBWT.Pfp

module TestHash = struct
  let is_trigger_string s =
    List.mem [ "AC"; "AG"; "T+"; "$$" ] s ~equal:String.( = )
end

module Parser = PFP (TestHash)

let paper_T = "GATTACAT+GATACAT+GATTAGATA"
let paper_BWT = "ATTTTTTCCGGGGAAA+$+AAATATAA"
let paper_RLBWT = [('A', 1); ('T', 6); ('C', 2); ('G', 4); ('A', 3); ('+', 1); ('$', 1); ('+', 1); ('A', 3); ('T', 1); ('A', 1); ('T', 1); ('A', 2);]
let paper_D = [ "$GATTAC"; "ACAT+"; "AGATA$$"; "T+GATAC"; "T+GATTAG" ]
let paper_parse = [ 0; 1; 3; 1; 4; 2 ]
let occ = [ 1; 2; 1; 1; 1 ]

let test_window _ =
  assert_equal (Parser.parse paper_T 2) (paper_D, occ, paper_parse)

let test_paper_bwt _ =
  assert_equal (Parser.getBWT paper_T 2) paper_BWT

let pfp_tests =
  "pfp_tests" >::: [ 
    "paper test" >:: test_window; 
    "paper bwt" >:: test_paper_bwt ]

let seq = In_channel.with_file "./data/big.txt" ~f:(fun channel -> In_channel.input_all channel)
let seq_BWT = In_channel.with_file "./data/big_bwt.txt" ~f:(fun channel -> In_channel.input_all channel)

let small = "GATTAGATACATGATACATGATTACAT"
let small_BWT = "TTTTTCGGCCGGAAAATT$ATAATAAAA"
let small_RLBWT = [('T', 5); ('C', 1); ('G', 2); ('C', 2); ('G', 2); ('A', 4); ('T', 2); ('$', 1); ('A', 1); ('T', 1); ('A', 2); ('T', 1); ('A', 4)]

module CharBWT = BigBWT.Naive_bwt.Text(BigBWT.Naive_bwt.CharSequence)
let test_naive_bwt _ =
  assert_equal (paper_T |> CharBWT.getBWT) paper_BWT;
  assert_equal (paper_T |> CharBWT.getSA |> CharBWT.bwt_from_SA paper_T) paper_BWT;
  assert_equal (paper_T |> CharBWT.getBWT |> CharBWT.rle_BWT) paper_RLBWT;

  assert_equal (small |> CharBWT.getBWT) small_BWT;
  assert_equal (small |> CharBWT.getSA |> CharBWT.bwt_from_SA small) small_BWT;
  assert_equal (small |> CharBWT.getBWT |> CharBWT.rle_BWT) small_RLBWT;

module SaisBWT = BigBWT.Sais.SAIS
let test_sais_bwt _ =
  assert_equal (paper_T |> SaisBWT.getBWT) paper_BWT;
  assert_equal (small |> SaisBWT.getBWT) small_BWT;
  assert_equal (seq |> SaisBWT.getBWT) seq_BWT

module PfpBWT = PFP (RollHash.Hash.DefaultHasher)
let test_pfp_bwt _ =
  assert_equal (PfpBWT.getBWT paper_T 10) paper_BWT;
  assert_equal (PfpBWT.getBWT small 10) small_BWT;
  assert_equal (PfpBWT.getBWT seq 10) seq_BWT

module GsacakBWT = BigBWT.Gsacak.GSACAK
let test_gsacak_bwt _ =
  assert_equal (paper_T |> GsacakBWT.getBWT) paper_BWT;
  assert_equal (small |> GsacakBWT.getBWT) small_BWT;
  assert_equal (seq |> GsacakBWT.getBWT) seq_BWT

let test_sais_sa _ =
  assert_equal ([|1; 2; 2; 1; 2; 2|] |> SaisBWT.getSA_int |> List.of_array) [6; 3; 0; 5; 2; 4; 1];
  assert_equal ([|2; 2; 2; 2|] |> SaisBWT.getSA_int |> List.of_array) [4; 3; 2; 1; 0]

let test_gsacak_sa _ =
  assert_equal ([|1; 2; 2; 1; 2; 2|] |> GsacakBWT.getSA_int |> List.of_array) [6; 3; 0; 5; 2; 4; 1];
  assert_equal ([|2; 2; 2; 2|] |> GsacakBWT.getSA_int |> List.of_array) [4; 3; 2; 1; 0]

let bwt_tests =
  "bwt_tests" >::: [ 
    "naive bwt" >:: test_naive_bwt;
    "sais bwt" >:: test_sais_bwt;
    "pfp bwt" >:: test_pfp_bwt;
    "gsacak bwt" >:: test_gsacak_bwt ]

let sa_int_tests =
    "sa_int_tests" >::: [
      "sais sa" >:: test_sais_sa;
      "gsacak sa" >:: test_gsacak_sa ]

let series = "Project Tests" >::: [ 
  pfp_tests;
  bwt_tests;
  sa_int_tests
 ]
let () = run_test_tt_main series

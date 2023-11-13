(*
  Put the tests for lib.ml functions here
*)

open Core;;
open OUnit2;;
open BigBWT.Pfp;;

module TestHash = struct
  let is_trigger s = List.mem ["AC"; "AG"; "T!"; "$$"] s ~equal:String.(=) 
end

module Parser = PFP(TestHash)
let paper_T = "GATTACAT!GATACAT!GATTAGATA"
let paper_D = [("$GATTAC", 1); ("ACAT!", 2); ("AGATA$$", 1); ("T!GATAC", 1); ("T!GATTAG", 1)]
let paper_parse = [0; 1; 3; 1; 4; 2]
let dict, parse = let d, p = (Parser.parse paper_T 2) in 
  (Parser.dict_to_alist d), p
let test_window _ = 
  assert_equal (dict, parse) (paper_D, paper_parse)
let tests = "tests" >::: [
  "paper test"    >:: test_window;
]

let series = "Project Tests" >::: [
  tests
]

let () = run_test_tt_main series
(*
  Put the tests for lib.ml functions here
*)

open Core;;
open OUnit2;;
open BigBWT.Pfp;;

module TestHash = struct
  let is_trigger _ s = List.mem ["AC"; "AG"; "T!"; "$$"] s ~equal:String.(=) 
end

module Parser = PFP(TestHash)
let paper_T = "GATTACAT!GATACAT!GATTAGATA"
let paper_D = ["T!GATTAG";"T!GATAC";"AGATA$$";"ACAT!";"$GATTAC"]
let paper_parse = [0; 1; 3; 1; 4; 2]
let test_window _ = 
  assert_equal (Parser.parse paper_T 2) ((Parser.buildDict paper_D), paper_parse)
let tests = "tests" >::: [
  "paper test"    >:: test_window;
]

let series = "Project Tests" >::: [
  tests
]

let () = run_test_tt_main series
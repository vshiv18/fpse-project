open Core
open OUnit2
open Serialize

let filename = "./data/serialize.test"

let test_int32_write _ =
  Int32Serializer.write filename 42;
  assert_equal 42 @@ Int32Serializer.read filename

let test_int32_write_list _ =
  Int32Serializer.write_list filename [ 42; 43; 44 ];
  assert_equal [ 42; 43; 44 ] @@ Int32Serializer.read_list filename

let test_int32_empty_read _ =
  assert_raises (Failure "empty file") @@ fun _ ->
  Int32Serializer.read "./data/empty.test"

let int32serializer_tests =
  "Int32Serializer tests"
  >: test_list
       [
         "test write" >:: test_int32_write;
         "test write_list" >:: test_int32_write_list;
         "test fail read" >:: test_int32_empty_read;
       ]

let test_string_write _ =
  StringSerializer.write filename "hello";
  assert_equal "hello" @@ StringSerializer.read filename

let test_string_write_list _ =
  StringSerializer.write_list filename [ "hello"; "world"; "!" ];
  assert_equal [ "hello"; "world"; "!" ] @@ StringSerializer.read_list filename

let stringserializer_tests =
  "StringSerializer tests"
  >: test_list
       [
         "test write" >:: test_string_write;
         "test write_list" >:: test_string_write_list;
       ]

let series =
  "Serializer tests" >::: [ int32serializer_tests; stringserializer_tests ]

let () = run_test_tt_main series

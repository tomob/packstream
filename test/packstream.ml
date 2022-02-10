open OUnit2
open Packstream

let test_null _ctx =
  let bs = Bitstring.bitstring_of_string "\xc0" in
  assert_equal (Ok Null) (parse bs)

let test_false _ctx =
  let bs = Bitstring.bitstring_of_string "\xc2" in
  assert_equal (Ok False) (parse bs)

let test_true _ctx =
  let bs = Bitstring.bitstring_of_string "\xc3" in
  assert_equal (Ok True) (parse bs)

let test_tiny_int_negative (str, v) =
  let bs = Bitstring.bitstring_of_string str in
  assert_equal (Ok (Int v)) (parse bs)

let test_tiny_int_positive i =
  let%bitstring bs = {| i : 8 |} in
  assert_equal (Ok (Int i)) (parse bs)

let test_tiny_int _ctx =
  let negative = [
    ("\xf0", -16);
    ("\xf1", -15);
    ("\xf2", -14);
    ("\xf3", -13);
    ("\xf4", -12);
    ("\xf5", -11);
    ("\xf6", -10);
    ("\xf7", -9);
    ("\xf8", -8);
    ("\xf9", -7);
    ("\xfa", -6);
    ("\xfb", -5);
    ("\xfc", -4);
    ("\xfd", -3);
    ("\xfe", -2);
    ("\xff", -1);
  ] in
  List.iter test_tiny_int_negative negative;
  List.iter test_tiny_int_positive (List.init 128 Fun.id)

let test_8_byte_int _ctx =
  let internal = fun i ->
    let%bitstring bs = {| 0xC8 : 8; i : 8 : signed,int |} in
    assert_equal (Ok (Int i)) (parse bs)
  in
  List.iter internal (List.init 256 (fun i -> i - 128))

let test_16_byte_int _ctx =
  let internal = fun i ->
    let%bitstring bs = {| 0xC9 : 8; i : 16 : signed,int |} in
    assert_equal (Ok (Int i)) (parse bs)
  in
  List.iter internal (List.init 65536 (fun i -> i - 32768))

let test_32_byte_int _ctx =
  let internal = fun (i:int32) ->
    let%bitstring bs = {| 0xCA : 8; i : 32 : int |} in
    assert_equal (Ok (Int (Int32.to_int i))) (parse bs)
  and examples = [
    -2147483648; -32769; 32768; 2147483647
  ]
  in
  List.iter internal (List.map Int32.of_int examples)

let test_64_byte_int _ctx =
  let internal = fun (i:int64) ->
    let%bitstring bs = {| 0xCB : 8; i : 64 : int |} in
    assert_equal (Ok (Int (Int64.to_int i))) (parse bs)
  and examples = [
    -9223372036854775808L; -2147483649L; 2147483648L; 9223372036854775807L
  ]
  in
  List.iter internal examples


let all_tests =
  "all_tests" >:::
  ["test_null" >:: test_null;
   "test_false" >:: test_false;
   "test_true" >:: test_true;
   "test_tiny_int" >:: test_tiny_int;
   "test_8_byte_int" >:: test_8_byte_int;
   "test_16_byte_int" >:: test_16_byte_int;
   "test_32_byte_int" >:: test_32_byte_int;
   "test_64_byte_int" >:: test_64_byte_int]

let () =
  run_test_tt_main all_tests

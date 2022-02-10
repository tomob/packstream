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
  let%bitstring bs = {| i : 8 : int,signed |} in
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
    let%bitstring bs = {| 0xC8 : 8; i : 8 : signed,int,bigendian |} in
    assert_equal (Ok (Int i)) (parse bs)
  in
  List.iter internal (List.init 256 (fun i -> i - 128))

let test_16_byte_int _ctx =
  let internal = fun i ->
    let%bitstring bs = {| 0xC9 : 8; i : 16 : signed,int,bigendian |} in
    assert_equal (Ok (Int i)) (parse bs)
  in
  List.iter internal (List.init 65536 (fun i -> i - 32768))

let test_32_byte_int _ctx =
  let internal = fun (i:int32) ->
    let%bitstring bs = {| 0xCA : 8; i : 32 : int,bigendian |} in
    assert_equal (Ok (Int (Int32.to_int i))) (parse bs)
  and examples = [
    -2147483648; -32769; 32768; 2147483647
  ]
  in
  List.iter internal (List.map Int32.of_int examples)

let test_64_byte_int _ctx =
  let internal = fun (i:int64) ->
    let%bitstring bs = {| 0xCB : 8; i : 64 : int,bigendian |} in
    assert_equal (Ok (Int (Int64.to_int i))) (parse bs)
  and examples = [
    -9223372036854775808L; -2147483649L; 2147483648L; 9223372036854775807L
  ]
  in
  List.iter internal examples

let test_float _ctx =
  let internal = fun f ->
    let i = Int64.bits_of_float f in
    let%bitstring bs = {| 0xC1 : 8; i : 64 : int,unsigned,bigendian |} in
    assert_equal (Ok (Float f)) (parse bs)
  and examples = [
    1.23; 0.; -123.456
  ]
  in
  List.iter internal examples

let test_incomplete_fails _ctx =
  let prefixes = [
    "\xc8";
    "\xc9"; "\xc9\x01";
    "\xca"; "\xca\x01"; "\xca\x01\x02"; "\xca\x01\x02\x03";
    "\xcb"; "\xcb\x01"; "\xcb\x01\x02"; "\xcb\x01\x02\x03"; "\xcb\x01\x02\x03\x04";
    "\xcb\x01\x02\x03\x04\x05"; "\xcb\x01\x02\x03\x04\x05\x06"; "\xcb\x01\x02\x03\x04\x05\x06\x07";
    "\xc1"; "\xc1\x01"; "\xc1\x01\x02"; "\xc1\x01\x02\x03"; "\xc1\x01\x02\x03\x04";
    "\xc1\x01\x02\x03\x04\x05"; "\xc1\x01\x02\x03\x04\x05\x06"; "\xc1\x01\x02\x03\x04\x05\x06\x07";
  ] in
  List.iter
    (fun prefix -> assert_equal (Error "Invalid message") (parse (Bitstring.bitstring_of_string prefix)))
    prefixes

let all_tests =
  "all_tests" >:::
  ["test_null" >:: test_null;
   "test_false" >:: test_false;
   "test_true" >:: test_true;
   "test_tiny_int" >:: test_tiny_int;
   "test_8_byte_int" >:: test_8_byte_int;
   "test_16_byte_int" >:: test_16_byte_int;
   "test_32_byte_int" >:: test_32_byte_int;
   "test_64_byte_int" >:: test_64_byte_int;
   "test_incomplete_fails" >:: test_incomplete_fails;
   "test_float" >:: test_float]

let () =
  run_test_tt_main all_tests

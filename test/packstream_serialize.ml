open OUnit2
open Packstream
open Core
open Packstream.Message

let run_test_cases cases =
  let internal = fun (expected, value) ->
    let bs = Bitstring.bitstring_of_string expected
    in assert_equal bs (serialize value)
  in
  List.iter ~f:internal cases

let test_null _ctx =
  run_test_cases [("\xc0", Null)]

let test_false _ctx =
  run_test_cases [("\xc2", False)]

let test_true _ctx =
  run_test_cases [("\xc3", True)]

let test_tiny_int _ctx =
  let neg_cases = [
    ("\xf0", Int (-16L));
    ("\xf1", Int (-15L));
    ("\xf2", Int (-14L));
    ("\xf3", Int (-13L));
    ("\xf4", Int (-12L));
    ("\xf5", Int (-11L));
    ("\xf6", Int (-10L));
    ("\xf7", Int (-9L));
    ("\xf8", Int (-8L));
    ("\xf9", Int (-7L));
    ("\xfa", Int (-6L));
    ("\xfb", Int (-5L));
    ("\xfc", Int (-4L));
    ("\xfd", Int (-3L));
    ("\xfe", Int (-2L));
    ("\xff", Int (-1L));
  ]
  and pos_cases = List.map (List.init 128 ~f:Fun.id) ~f:(fun n ->
    (String.of_char @@ Char.of_int_exn n, Int (Int64.of_int_exn n)))
  in
  run_test_cases @@ List.concat [neg_cases; pos_cases]

let test_8_byte_int _ctx =
  let numbers = List.init ~f:(fun x -> x - 128) 112 in
  let cases = List.map numbers ~f:(fun i ->
    ("\xc8"^(String.of_char @@ Char.of_int_exn @@ 256 + i), Int (Int64.of_int i)))
  in run_test_cases cases

let test_16_byte_int _ctx =
  let cases = [
    ("\xC9\x80\x00", Int (-32768L));
    ("\xC9\xff\x7f", Int (-129L));
    ("\xC9\x00\x80", Int 128L);
    ("\xC9\x7f\xff", Int 32767L)
  ]
  in
  run_test_cases cases

let test_32_byte_int _ctx =
  let cases = [
    ("\xCA\x80\x00\x00\x00", Int (-2147483648L));
    ("\xCA\xff\xff\x7f\xff", Int (-32769L));
    ("\xCA\x00\x00\x80\x00", Int 32768L);
    ("\xCA\x7f\xff\xff\xff", Int 2147483647L)
  ]
  in
  run_test_cases cases

let test_64_byte_int _ctx =
  let cases = [
    ("\xCB\128\000\000\000\000\000\000\000", Int (-9223372036854775808L));
    ("\xCB\255\255\255\255\127\255\255\255", Int (-2147483649L));
    ("\xCB\000\000\000\000\128\000\000\000", Int 2147483648L);
    ("\xCB\127\255\255\255\255\255\255\255", Int 9223372036854775807L)
  ]
  in
  run_test_cases cases

let test_float _ctx =
  run_test_cases [("\xC1\x3F\xF3\xAE\x14\x7A\xE1\x47\xAE", Float 1.23)]

let test_bytes _ctx =
  let cases = [
    ("\xCC\x00", Bytes (Bytes.of_string ""));
    ("\xCC\x03\x01\x02\x03", Bytes (Bytes.of_string "\x01\x02\x03"));
    ("\xCD\x01\x00"^(String.make 256 '\x00'), Bytes (Bytes.make 256 '\x00'));
    ("\xCE\x00\x01\x00\x00"^(String.make 65536 '\x00'), Bytes (Bytes.make 65536 '\x00'));
  ] in
  run_test_cases cases

let test_strings _ctx =
  let cases = [
    ("\x80", String "");
    ("\x81a", String "a");
    ("\x82ab", String "ab");
    ("\x83abc", String "abc");
    ("\x84abcd", String "abcd");
    ("\x85abcde", String "abcde");
    ("\x86abcdef", String "abcdef");
    ("\x87abcdefg", String "abcdefg");
    ("\x88abcdefgh", String "abcdefgh");
    ("\x89abcdefghi", String "abcdefghi");
    ("\x8aabcdefghij", String "abcdefghij");
    ("\x8babcdefghijk", String "abcdefghijk");
    ("\x8cabcdefghijkl", String "abcdefghijkl");
    ("\x8dabcdefghijklm", String "abcdefghijklm");
    ("\x8eabcdefghijklmn", String "abcdefghijklmn");
    ("\x8fabcdefghijklmno", String "abcdefghijklmno");
    ("\xD0\x12\x47\x72\xC3\xB6\xC3\x9F\x65\x6E\x6D\x61\xC3\x9F\x73\x74\xC3\xA4\x62\x65", String "Größenmaßstäbe")
  ] in
  run_test_cases cases

let test_lists _ctx =
  let cases = [
    ("\x90", List []);
    ("\x93\x01\x02\x03", List [Int 1L; Int 2L; Int 3L]);
    ("\x93\x01\xC1\x40\x00\x00\x00\x00\x00\x00\x00\x85three", List [Int 1L; Float 2.0; String "three"]);
    ("\xD4\x28\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x0D\x0E\x0F\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1A\x1B\x1C\x1D\x1E\x1F\x20\x21\x22\x23\x24\x25\x26\x27\x28",
      List (List.map ~f:(fun x -> Int x) (List.init 40 ~f:(fun x -> Int64.of_int (x + 1)))))
  ] in
  run_test_cases cases

let test_dictionaries _ctx =
  let cases = [
    ("\xA0", Dict []);
    ("\xA1\x83\x6F\x6E\x65\x84\x65\x69\x6E\x73", Dict ["one", String "eins"]);
    ("\xD8\x1A\x81\x41\x01\x81\x42\x02\x81\x43\x03\x81\x44\x04" ^
      "\x81\x45\x05\x81\x46\x06\x81\x47\x07\x81\x48\x08\x81\x49" ^
      "\x09\x81\x4A\x0A\x81\x4B\x0B\x81\x4C\x0C\x81\x4D\x0D\x81" ^
      "\x4E\x0E\x81\x4F\x0F\x81\x50\x10\x81\x51\x11\x81\x52\x12" ^
      "\x81\x53\x13\x81\x54\x14\x81\x55\x15\x81\x56\x16\x81\x57" ^
      "\x17\x81\x58\x18\x81\x59\x19\x81\x5A\x1A",
      Dict ["A", Int 1L; "B", Int 2L; "C", Int 3L; "D", Int 4L; "E", Int 5L; "F", Int 6L; "G", Int 7L;
            "H", Int 8L; "I", Int 9L; "J", Int 10L; "K", Int 11L; "L", Int 12L; "M", Int 13L; "N", Int 14L;
            "O", Int 15L; "P", Int 16L; "Q", Int 17L; "R", Int 18L; "S", Int 19L; "T", Int 20L; "U", Int 21L;
            "V", Int 22L; "W", Int 23L; "X", Int 24L; "Y", Int 25L; "Z", Int 26L])
  ] in
  run_test_cases cases

let test_structs _ctx =
  let cases = [
    ("\xB0\x02", Struct {length = 0; tag = 2; fields = []});
    ("\xB1\x01\x00", Struct {length = 1; tag = 1; fields = [Int 0L]});
    ("\xB2\x03\x00\x80", Struct {length = 2; tag = 3; fields = [Int 0L; String ""]});
    ("\xB3\x03\x00\x80\x90", Struct {length = 3; tag = 3; fields = [Int 0L; String ""; List []]});
    ("\xB4\x02\x00\x80\x90\x00",
     Struct {length = 4;  tag = 2; fields = [Int 0L; String ""; List []; Int 0L]});
    ("\xB5\x02\x00\x80\x90\x00\x80",
     Struct {length = 5;  tag = 2; fields = [Int 0L; String ""; List []; Int 0L; String ""]});
    ("\xB6\x02\x00\x80\x90\x00\x80\x90",
     Struct {length = 6;  tag = 2; fields = [Int 0L; String ""; List []; Int 0L; String ""; List []]});
    ("\xB7\x02\x00\x80\x90\x00\x80\x90\x00",
     Struct {length = 7;  tag = 2; fields = [Int 0L; String ""; List []; Int 0L; String ""; List []; Int 0L]});
    ("\xB8\x02\x00\x80\x90\x00\x80\x90\x00\x80",
     Struct {length = 8;  tag = 2; fields = [Int 0L; String ""; List []; Int 0L; String ""; List []; Int 0L; String ""]});
    ("\xB9\x02\x00\x80\x90\x00\x80\x90\x00\x80\x90",
     Struct {length = 9;  tag = 2; fields = [Int 0L; String ""; List []; Int 0L; String ""; List []; Int 0L; String ""; List []]});
    ("\xBA\x02\x00\x80\x90\x00\x80\x90\x00\x80\x90\x00",
     Struct {length = 10; tag = 2; fields = [
      Int 0L; String ""; List []; Int 0L; String ""; List []; Int 0L; String ""; List []; Int 0L;
     ]});
    ("\xBB\x02\x00\x80\x90\x00\x80\x90\x00\x80\x90\x00\x80",
     Struct {length = 11; tag = 2; fields = [
      Int 0L; String ""; List []; Int 0L; String ""; List []; Int 0L; String ""; List []; Int 0L; String ""
     ]});
    ("\xBC\x02\x00\x80\x90\x00\x80\x90\x00\x80\x90\x00\x80\x90",
     Struct {length = 12; tag = 2; fields = [
      Int 0L; String ""; List []; Int 0L; String ""; List []; Int 0L; String ""; List []; Int 0L; String ""; List []
     ]});
    ("\xBD\x02\x00\x80\x90\x00\x80\x90\x00\x80\x90\x00\x80\x90\x00",
     Struct {length = 13; tag = 2; fields = [
      Int 0L; String ""; List []; Int 0L; String ""; List []; Int 0L; String ""; List []; Int 0L; String ""; List [];
      Int 0L
     ]});
    ("\xBE\x02\x00\x80\x90\x00\x80\x90\x00\x80\x90\x00\x80\x90\x00\x80",
     Struct {length = 14; tag = 2; fields = [
      Int 0L; String ""; List []; Int 0L; String ""; List []; Int 0L; String ""; List []; Int 0L; String ""; List [];
      Int 0L; String ""
     ]});
    ("\xBF\x02\x00\x80\x90\x00\x80\x90\x00\x80\x90\x00\x80\x90\x00\x80\x90",
     Struct {length = 15; tag = 2; fields = [
      Int 0L; String ""; List []; Int 0L; String ""; List []; Int 0L; String ""; List []; Int 0L; String ""; List [];
      Int 0L; String ""; List []
     ]});
    ("\xB3\x4E\x01\x90\xA0", Node {id = 1L; labels = []; properties = []});
    ("\xB5\x52\x0B\x02\x03\x85KNOWS\xA1\x84name\x87example",
      Relationship {id = 11L; start_node_id = 2L; end_node_id = 3L; typ = "KNOWS"; properties = ["name", String "example"]});
    ("\xB3\x72\x0B\x85KNOWS\xA1\x84name\x87example",
      UnboundRelationship {id = 11L; typ = "KNOWS"; properties = ["name", String "example"]});
    ("\xB3\x50\x91\xB3\x4E\x01\x90\xA0\x91\xB3\x72\x0B\x85KNOWS\xA1\x84name\x87example\x91\x01",
      Path {nodes = [{id = 1L; labels = []; properties = []}];
            rels = [{id = 11L; typ = "KNOWS"; properties = ["name", String "example"]}];
            ids = [1L]});
    ("\xB1\x44\x02", Date {days = 2L});
    ("\xB2\x54\x02\x03", Time {nanoseconds = 2L; tz_offset_seconds = 3L});
    ("\xB1\x74\x05", LocalTime {nanoseconds = 5L});
    ("\xB3\x46\x02\x03\x04", DateTime {seconds = 2L; nanoseconds = 3L; tz_offset_seconds = 4L});
    ("\xB3\x66\x02\x03\x8CEurope/Paris", DateTimeZoneId {seconds = 2L; nanoseconds = 3L; tz_id = "Europe/Paris"});
    ("\xB2\x64\x02\x03", LocalDateTime {seconds = 2L; nanoseconds = 3L});
    ("\xB4\x45\x02\x03\x04\x05", Duration {months = 2L; days = 3L; seconds = 4L; nanoseconds = 5L});
    ("\xB3\x58\x01\xC1\x3F\xF3\xAE\x14\x7A\xE1\x47\xAE\xC1\x3F\xF3\xAE\x14\x7A\xE1\x47\xAE",
      Point2D {srid = 1L; x = 1.23; y = 1.23});
      ("\xB4\x59\x01\xC1\x3F\xF3\xAE\x14\x7A\xE1\x47\xAE\xC1\x3F\xF3\xAE\x14\x7A\xE1\x47\xAE" ^
      "\xC1\x3F\xF3\xAE\x14\x7A\xE1\x47\xAE",
      Point3D {srid = 1L; x = 1.23; y = 1.23; z = 1.23})
  ] in
  run_test_cases cases
let all_tests =
  "Serialize tests" >:::
  ["test_null" >:: test_null;
  "test_false" >:: test_false;
  "test_true" >:: test_true;
  "test_tiny_int" >:: test_tiny_int;
  "test_8_byte_int" >:: test_8_byte_int;
  "test_16_byte_int" >:: test_16_byte_int;
  "test_32_byte_int" >:: test_32_byte_int;
  "test_64_byte_int" >:: test_64_byte_int;
  "test_float" >:: test_float;
  "test_bytes" >:: test_bytes;
  "test_strings" >:: test_strings;
  "test_lists" >:: test_lists;
  "test_dictionaries" >:: test_dictionaries;
  "test_structs" >:: test_structs
  ]

let () =
  run_test_tt_main all_tests

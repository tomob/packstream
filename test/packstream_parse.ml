open OUnit2
open Packstream
open Core
open Packstream.Message

let run_test_cases cases =
  let internal = fun (s, v) ->
    let bs = Bitstring.bitstring_of_string s
    in
      (* match (parse bs) with
      | Ok x -> print_string (Message.show x); Out_channel.newline stdout
      | Error err -> print_string ("ERROR: " ^ err); Out_channel.newline stdout; *)
      assert_equal (Ok v) (parse bs)
  in
  List.iter ~f:internal cases

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
  assert_equal (Ok (Int (Int64.of_int i))) (parse bs)

let test_tiny_int _ctx =
  let negative = [
    ("\xf0", -16L);
    ("\xf1", -15L);
    ("\xf2", -14L);
    ("\xf3", -13L);
    ("\xf4", -12L);
    ("\xf5", -11L);
    ("\xf6", -10L);
    ("\xf7", -9L);
    ("\xf8", -8L);
    ("\xf9", -7L);
    ("\xfa", -6L);
    ("\xfb", -5L);
    ("\xfc", -4L);
    ("\xfd", -3L);
    ("\xfe", -2L);
    ("\xff", -1L);
  ] in
  List.iter ~f:test_tiny_int_negative negative;
  List.iter ~f:test_tiny_int_positive (List.init 128 ~f:Fun.id)

let test_8_byte_int _ctx =
  let internal = fun i ->
    let%bitstring bs = {| 0xC8 : 8; i : 8 : signed,int,bigendian |} in
    assert_equal (Ok (Int (Int64.of_int i))) (parse bs)
  in
  List.iter ~f:internal (List.init 256 ~f:(fun i -> i - 128))

let test_16_byte_int _ctx =
  let internal = fun i ->
    let%bitstring bs = {| 0xC9 : 8; i : 16 : signed,int,bigendian |} in
    assert_equal (Ok (Int (Int64.of_int i))) (parse bs)
  in
  List.iter ~f:internal (List.init 65536 ~f:(fun i -> i - 32768))

let test_32_byte_int _ctx =
  let internal = fun (i:int32) ->
    let%bitstring bs = {| 0xCA : 8; i : 32 : int,bigendian |} in
    assert_equal (Ok (Int (Int64.of_int32 i))) (parse bs)
  and examples = [
    -2147483648; -32769; 32768; 2147483647
  ]
  in
  List.iter ~f:internal (List.map ~f:Int32.of_int_exn examples)

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
  let internal = fun f ->
    let i = Int64.bits_of_float f in
    let%bitstring bs = {| 0xC1 : 8; i : 64 : int,unsigned,bigendian |} in
    assert_equal (Ok (Float f)) (parse bs)
  and examples = [
    1.23; 0.; -123.456
  ]
  in
  List.iter ~f:internal examples

let test_bytes _ctx =
  let cases = [
    ("\xCC\x00", Bytes (Bytes.of_string ""));
    ("\xCC\x03\x01\x02\x03", Bytes (Bytes.of_string "\x01\x02\x03"));
    ("\xCD\x00\x00", Bytes (Bytes.of_string ""));
    ("\xCD\x00\x03\x01\x02\x03", Bytes (Bytes.of_string "\x01\x02\x03"));
    ("\xCE\x00\x00\x00\x00", Bytes (Bytes.of_string ""));
    ("\xCE\x00\x00\x00\x03\x01\x02\x03", Bytes (Bytes.of_string "\x01\x02\x03"));
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
    ("\xD0\x00", String "");
    ("\xD0\x01a", String "a");
    ("\xD1\x00\x00", String "");
    ("\xD1\x00\x01a", String "a");
    ("\xD2\x00\x00\x00\x00", String "");
    ("\xD2\x00\x00\x00\x01a", String "a");
    ("\xD0\x12\x47\x72\xC3\xB6\xC3\x9F\x65\x6E\x6D\x61\xC3\x9F\x73\x74\xC3\xA4\x62\x65", String "Größenmaßstäbe")
  ] in
  run_test_cases cases

let test_lists _ctx =
  let cases = [
    ("\x90", List []);
    ("\x93\x01\x02\x03", List [Int 1L; Int 2L; Int 3L]);
    ("\x93\x01\xC1\x40\x00\x00\x00\x00\x00\x00\x00\x85\x74\x68\x72\x65\x65", List [Int 1L; Float 2.0; String "three"]);
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

let test_incomplete_fails _ctx =
  let prefixes = [
    (* Numbers *)
    "\xc8";
    "\xc9"; "\xc9\x01";
    "\xca"; "\xca\x01"; "\xca\x01\x02"; "\xca\x01\x02\x03";
    "\xcb"; "\xcb\x01"; "\xcb\x01\x02"; "\xcb\x01\x02\x03"; "\xcb\x01\x02\x03\x04";
    "\xcb\x01\x02\x03\x04\x05"; "\xcb\x01\x02\x03\x04\x05\x06"; "\xcb\x01\x02\x03\x04\x05\x06\x07";
    "\xc1"; "\xc1\x01"; "\xc1\x01\x02"; "\xc1\x01\x02\x03"; "\xc1\x01\x02\x03\x04";
    "\xc1\x01\x02\x03\x04\x05"; "\xc1\x01\x02\x03\x04\x05\x06"; "\xc1\x01\x02\x03\x04\x05\x06\x07";
    "\xcc"; "\xcc\x01"; "\xcc\x02\x00";
    "\xcd"; "\xcd\x00"; "\xcd\x00\x01"; "\xcd\x00\x02\x00";
    "\xce"; "\xce\x00"; "\xce\x00\x00\x00\x01";
    (* Strings *)
    "\x81"; "\x82a"; "\x83ab"; "\x84abc"; "\x85abcd"; "\x86abcde"; "\x87abcdef";
    "\x88abcdefg"; "\x89abcdefgh"; "\x8aabcdefghi"; "\x8babcdefghij"; "\x8cabcdefghijk";
    "\x8dabcdefghijkl"; "\x8eabcdefghijklm"; "\x8fabcdefghijklmn";
    "\xD0\x01"; "\xD1\x00\x01"; "\xD2\x00\x00\x00\x01";
    (* Lists *)
    "\x91"; "\x92\x01"; "\x93\x01\x01"; "\x94\x01\x01\x01"; "\x95\x01\x01\x01\x01";
    "\x96\x01\x01\x01\x01\x01"; "\x97\x01\x01\x01\x01\x01\x01"; "\x98\x01\x01\x01\x01\x01\x01\x01";
    "\x99\x01\x01\x01\x01\x01\x01\x01\x01"; "\x9a\x01\x01\x01\x01\x01\x01\x01\x01\x01";
    "\x9b\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01"; "\x9c\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01";
    "\x9d\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01";
    "\x9e\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01";
    "\x9f\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01";
    "\xd4"; "\xd4\x01";
    "\xd5"; "\xd5\x01"; "\xd5\x00\x01";
    "\xd6"; "\xd6\x01"; "\xd6\x00\x00\x00\x01"
  ] in
  List.iter
    ~f:(fun prefix -> assert_equal (Error "Invalid message") (parse (Bitstring.bitstring_of_string prefix)))
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
   "test_float" >:: test_float;
   "test_bytes" >:: test_bytes;
   "test_strings" >:: test_strings;
   "test_lists" >:: test_lists;
   "test_dictionaries" >:: test_dictionaries;
   "test_structs" >:: test_structs]

let () =
  run_test_tt_main all_tests

type message =
  | Null
  | True
  | False
  | Int of int
  | Float of float
  | Bytes of string

let parse bitstring =
  match%bitstring bitstring with
  | {| 0xC0 : 8 |} -> Ok Null
  | {| 0xC2 : 8 |} -> Ok False
  | {| 0xC3 : 8 |} -> Ok True
  (* Float *)
  | {| 0xC1 : 8; i : 64 |} -> Ok (Float (Int64.float_of_bits i))
  (* Prefixed ints *)
  | {| 0xC8 : 8; i :  8 : signed |} -> Ok (Int i)
  | {| 0xC9 : 8; i : 16 : signed |} -> Ok (Int i)
  | {| 0xCA : 8; i : 32 |}          -> Ok (Int (Int32.to_int i))
  | {| 0xCB : 8; i : 64 |}          -> Ok (Int (Int64.to_int i))
  (* Small ints *)
  | {| flag : 1; i : 7 |} when not flag -> Ok (Int i)
  | {| 0x0F : 4; i : 4 |}               -> Ok (Int (i-16))
  (* Byte array *)
  | {| 0xCC : 8; length :  8 ; bytes : length*8 : string |} -> Ok (Bytes bytes)
  | {| 0xCD : 8; length : 16 ; bytes : length*8 : string |} -> Ok (Bytes bytes)
  | {| 0xCE : 8; length : 32 ; bytes : (Int32.to_int length)*8 : string |} -> Ok (Bytes bytes)
  (* Failure case *)
  | {| _ |} -> Error "Invalid message"

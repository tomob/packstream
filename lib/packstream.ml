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
  | {| 0xC1 : 8; i : 64 : int, unsigned,bigendian |} -> Ok (Float (Int64.float_of_bits i))
  | {| 0xC2 : 8 |} -> Ok False
  | {| 0xC3 : 8 |} -> Ok True
  | {| 0xC8 : 8; i : 8 : int,signed,bigendian |} -> Ok (Int i)
  | {| 0xC9 : 8; i : 16 : int,signed,bigendian |} -> Ok (Int i)
  | {| 0xCA : 8; i : 32 : int,bigendian |} -> Ok (Int (Int32.to_int i))
  | {| 0xCB : 8; i : 64 : int,bigendian |} -> Ok (Int (Int64.to_int i))
  | {| 0xCC : 8; length : 8 : int,unsigned; bytes : length*8 : string |} -> Ok (Bytes bytes)
  | {| 0xCD : 8; length : 16 : int,unsigned; bytes : length*8 : string |} -> Ok (Bytes bytes)
  | {| 0xCE : 8; length : 32 : int,unsigned; bytes : (Int32.to_int length)*8 : string |} -> Ok (Bytes bytes)
  | {| flag : 1; i : 7 : int,unsigned |} when not flag -> Ok (Int i)
  | {| 0xF : 4; i : 4 : int,unsigned |} -> Ok (Int (i-16))
  | {| _ |} -> Error "Invalid message"

type message =
  | Null
  | True
  | False
  | Int of int

let parse bitstring =
  match%bitstring bitstring with
  | {| 0xC0 : 8 |} -> Ok Null
  | {| 0xC2 : 8 |} -> Ok False
  | {| 0xC3 : 8 |} -> Ok True
  | {| 0xC8 : 8; i : 8 : int,signed |} -> Ok (Int i)
  | {| 0xC9 : 8; i : 16 : int,signed |} -> Ok (Int i)
  | {| 0xCA : 8; i : 32 : int |} -> Ok (Int (Int32.to_int i))
  | {| 0xCB : 8; i : 64 : int |} -> Ok (Int (Int64.to_int i))
  | {| i : 8 : int,signed |} -> Ok (Int i)
  | {| _ |} -> Error "Invalid message"

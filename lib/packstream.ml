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
  | {| i : 8 : int,signed |} -> Ok (Int i)
  | {| _ |} -> Error "Invalid message"

open Core

type message =
  | Null
  | True
  | False
  | Int of int64
  | Float of float
  | Bytes of string
  | String of string
  | List of message list
  | Dict of (string * message) list
[@@deriving show]

let rec parse_one bitstring =
  match%bitstring bitstring with
  | {| 0xC0 : 8 ; rest : -1 : bitstring |} -> Ok Null, rest
  | {| 0xC2 : 8 ; rest : -1 : bitstring |} -> Ok False, rest
  | {| 0xC3 : 8 ; rest : -1 : bitstring |} -> Ok True, rest
  (* Float *)
  | {| 0xC1 : 8; i : 64 ; rest : -1 : bitstring |} -> Ok (Float (Int64.float_of_bits i)), rest
  (* Prefixed ints *)
  | {| 0xC8 : 8; i :  8 : signed ; rest : -1 : bitstring |} -> Ok (Int (Int64.of_int i)), rest
  | {| 0xC9 : 8; i : 16 : signed ; rest : -1 : bitstring |} -> Ok (Int (Int64.of_int i)), rest
  | {| 0xCA : 8; i : 32 ; rest : -1 : bitstring |}          -> Ok (Int (Int64.of_int32 i)), rest
  | {| 0xCB : 8; i : 64 ; rest : -1 : bitstring |}          -> Ok (Int i), rest
  (* Small ints *)
  | {| flag : 1; i : 7 ; rest : -1 : bitstring |} when not flag -> Ok (Int (Int64.of_int i)), rest
  | {| 0x0F : 4; i : 4 ; rest : -1 : bitstring |}               -> Ok (Int (Int64.of_int (i-16))), rest
  (* Byte array *)
  | {| 0xCC : 8; length :  8 ; bytes : length*8 : string ; rest : -1 : bitstring |} -> Ok (Bytes bytes), rest
  | {| 0xCD : 8; length : 16 ; bytes : length*8 : string ; rest : -1 : bitstring |} -> Ok (Bytes bytes), rest
  | {| 0xCE : 8; length : 32 ; bytes : (Int32.to_int_exn length)*8 : string ; rest : -1 : bitstring |}
    -> Ok (Bytes bytes), rest
  (* Strings *)
  | {| 0x8  : 4; length :  4 : unsigned ; str : length*8 : string ; rest : -1 : bitstring |} -> Ok (String str), rest
  | {| 0xD0 : 8; length :  8 : unsigned ; str : length*8 : string ; rest : -1 : bitstring |} -> Ok (String str), rest
  | {| 0xD1 : 8; length : 16 : unsigned ; str : length*8 : string ; rest : -1 : bitstring |} -> Ok (String str), rest
  | {| 0xD2 : 8; length : 32 : unsigned ; str : (Int32.to_int_exn length)*8 : string ; rest : -1 : bitstring |}
    -> Ok (String str), rest
  (* Lists *)
  | {| 0x9  : 4; length :  4 : unsigned ; data : -1 : bitstring |} -> parse_list length data
  | {| 0xD4 : 8; length :  8 : unsigned ; data : -1 : bitstring |} -> parse_list length data
  | {| 0xD5 : 8; length : 16 : unsigned ; data : -1 : bitstring |} -> parse_list length data
  | {| 0xD6 : 8; length : 32 : unsigned ; data : -1 : bitstring |} -> parse_list (Int32.to_int_exn length) data
  (* Dictionaries *)
  | {| 0xA  : 4; length :  4 : unsigned ; data : -1 : bitstring |} -> parse_dict length data
  | {| 0xD8 : 8; length :  8 : unsigned ; data : -1 : bitstring |} -> parse_dict length data
  | {| 0xD9 : 8; length : 16 : unsigned ; data : -1 : bitstring |} -> parse_dict length data
  | {| 0xDA : 8; length : 32 : unsigned ; data : -1 : bitstring |} -> parse_dict (Int32.to_int_exn length) data
  (* Failure case *)
  | {| _ |} -> Error "Invalid message", bitstring
and parse_list length data =
  let internal = fun (l, bitstring) _ ->
    let r, rst = parse_one bitstring in r::l, rst
  in
  let fake_list = List.init length ~f:Fn.id in
  let result, rest = List.fold fake_list ~init:([], data) ~f:internal in
  let result = List.rev result
    |> Result.all
    |> Result.map ~f:(fun x -> List x)
  in result, rest
and parse_dict length data =
  let internal = fun (d, bitstring) _ ->
    match parse_one bitstring with
    | Ok (String s), rst ->
        let open Result in
        let v, rst = parse_one rst in
        (v >>= fun vv -> Ok (s,vv))::d, rst
    | _ -> [Error "Key must be a string"], bitstring
  in
  let fake_list = List.init length ~f:Fn.id in
  let result, rest = List.fold fake_list ~init:([], data) ~f:internal in
  let result = List.rev result
    |> Result.all
    |> Result.map ~f:(fun x -> Dict x)
  in
    result, rest

let parse bitsting =
  let result, _ = parse_one bitsting
  in result

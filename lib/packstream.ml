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
  | Node of message * message * message
  | Relationship of message * message * message * message * message
  | UnboundRelationship of message * message * message
  | Path of message * message * message
  | Date of message
  | Time of message * message
  | LocalTime of message
  | DateTime of message * message * message
  | DateTimeZoneId of message * message * message
  | LocalDateTime of message * message
  | Duration of message * message * message * message
  | Point2D of message * message * message
  | Point3D of message * message * message * message
[@@deriving show]

let cons lst elem =
  List.cons elem lst

let rec parse_one (bitstring:Bitstring.t) =
  match%bitstring bitstring with
  | {| 0xC0 : 8; rest : -1 : bitstring |} -> Ok Null, rest
  | {| 0xC2 : 8; rest : -1 : bitstring |} -> Ok False, rest
  | {| 0xC3 : 8; rest : -1 : bitstring |} -> Ok True, rest
  (* Float *)
  | {| 0xC1 : 8; i : 64; rest : -1 : bitstring |} -> Ok (Float (Int64.float_of_bits i)), rest
  (* Prefixed ints *)
  | {| 0xC8 : 8; i :  8 : signed; rest : -1 : bitstring |} -> Ok (Int (Int64.of_int i)), rest
  | {| 0xC9 : 8; i : 16 : signed; rest : -1 : bitstring |} -> Ok (Int (Int64.of_int i)), rest
  | {| 0xCA : 8; i : 32         ; rest : -1 : bitstring |} -> Ok (Int (Int64.of_int32 i)), rest
  | {| 0xCB : 8; i : 64         ; rest : -1 : bitstring |} -> Ok (Int i), rest
  (* Small ints *)
  | {| flag : 1; i : 7; rest : -1 : bitstring |} when not flag -> Ok (Int (Int64.of_int i)), rest
  | {| 0x0F : 4; i : 4; rest : -1 : bitstring |}               -> Ok (Int (Int64.of_int (i-16))), rest
  (* Byte array *)
  | {| 0xCC : 8; length :  8; bytes : length*8 : string; rest : -1 : bitstring |} -> Ok (Bytes bytes), rest
  | {| 0xCD : 8; length : 16; bytes : length*8 : string; rest : -1 : bitstring |} -> Ok (Bytes bytes), rest
  | {| 0xCE : 8; length : 32; bytes : (Int32.to_int_exn length)*8 : string; rest : -1 : bitstring |}
    -> Ok (Bytes bytes), rest
  (* Strings *)
  | {| 0x8  : 4; length :  4 : unsigned; str : length*8 : string; rest : -1 : bitstring |} -> Ok (String str), rest
  | {| 0xD0 : 8; length :  8 : unsigned; str : length*8 : string; rest : -1 : bitstring |} -> Ok (String str), rest
  | {| 0xD1 : 8; length : 16 : unsigned; str : length*8 : string; rest : -1 : bitstring |} -> Ok (String str), rest
  | {| 0xD2 : 8; length : 32 : unsigned; str : (Int32.to_int_exn length)*8 : string; rest : -1 : bitstring |}
    -> Ok (String str), rest
  (* Lists *)
  | {| 0x9  : 4; length :  4 : unsigned; data : -1 : bitstring |} -> parse_list length data
  | {| 0xD4 : 8; length :  8 : unsigned; data : -1 : bitstring |} -> parse_list length data
  | {| 0xD5 : 8; length : 16 : unsigned; data : -1 : bitstring |} -> parse_list length data
  | {| 0xD6 : 8; length : 32 : unsigned; data : -1 : bitstring |} -> parse_list (Int32.to_int_exn length) data
  (* Dictionaries *)
  | {| 0xA  : 4; length :  4 : unsigned; data : -1 : bitstring |} -> parse_dict length data
  | {| 0xD8 : 8; length :  8 : unsigned; data : -1 : bitstring |} -> parse_dict length data
  | {| 0xD9 : 8; length : 16 : unsigned; data : -1 : bitstring |} -> parse_dict length data
  | {| 0xDA : 8; length : 32 : unsigned; data : -1 : bitstring |} -> parse_dict (Int32.to_int_exn length) data
  (* Structs  *)
  | {| 0xB : 4; length : 4 : unsigned; tag : 8 : unsigned; data : -1 : bitstring |} -> parse_structs length tag data
  (* Failure case *)
  | {| _ |} -> Error "Invalid message", bitstring

and parse_list length (data:Bitstring.t) =
  let open Tuple2 in
  let internal = fun (l, bitstring) _ ->
    map_fst (parse_one bitstring) ~f:(cons l)
  in
  let fake_list = List.init length ~f:Fn.id in
  List.fold fake_list ~init:([], data) ~f:internal
  |> map_fst ~f:List.rev
  |> map_fst ~f:(Result.all)
  |> map_fst ~f:(Result.map ~f:(fun x -> List x))

and parse_dict length (data:Bitstring.t) =
  let open Tuple2 in
  let open Result in
  let internal = fun (d, bitstring) _ ->
    match parse_one bitstring with
    | Ok (String s), rst ->
        map_fst (parse_one rst) ~f:(fun v -> cons d (v >>= fun vv -> Ok (s,vv)))
    | _ -> [Error "Key must be a string"], bitstring
  in
  let fake_list = List.init length ~f:Fn.id in
  List.fold fake_list ~init:([], data) ~f:internal
  |> map_fst ~f:List.rev
  |> map_fst ~f:Result.all
  |> map_fst ~f:(Result.map ~f:(fun x -> Dict x))

and parse_structs length tag (data:Bitstring.t) =
  match tag with
  | 0x4E when length = 3 -> parse_fields length data
    (function [i; l; p] -> Ok (Node (i, l, p)) | _ -> Error "Cound not parse Node")
  | 0x52 when length = 5 -> parse_fields length data
    (function [i; s; e; t; p] -> Ok (Relationship (i, s, e, t, p)) | _ -> Error "Cound not parse Relationship")
  | 0x72 when length = 3 -> parse_fields length data
    (function [i; t; p] ->  Ok (UnboundRelationship (i, t, p)) | _ -> Error "Cound not parse UnboundRelationship")
  | 0x50 when length = 3 -> parse_fields length data
    (function [n; r; p] -> Ok (Path (n, r, p)) | _ -> Error "Cound not parse Path")
  | 0x44 when length = 1 -> parse_fields length data
    (function [d] -> Ok (Date d) | _ -> Error "Cound not parse Date")
  | 0x54 when length = 2 -> parse_fields length data
    (function [n; t] -> Ok (Time (n, t)) | _ -> Error "Cound not parse Time")
  | 0x74 when length = 1 -> parse_fields length data
    (function [t] -> Ok (LocalTime t) | _ -> Error "Cound not parse LocalTime")
  | 0x46 when length = 3 -> parse_fields length data
    (function [s; n; t] -> Ok (DateTime (s, n, t)) | _ -> Error "Cound not parse DateTime")
  | 0x66 when length = 3 -> parse_fields length data
    (function [s; n; t] -> Ok (DateTimeZoneId (s, n, t)) | _ -> Error "Cound not parse DateTimeZoneId")
  | 0x64 when length = 2 -> parse_fields length data
    (function [s; n] -> Ok (LocalDateTime (s, n)) | _ -> Error "Cound not parse LocalDateTime")
  | 0x45 when length = 4 -> parse_fields length data
    (function [m; d; s; n] -> Ok (Duration (m, d, s, n)) | _ -> Error "Cound not parse Duration")
  | 0x58 when length = 3 -> parse_fields length data
    (function [s; x; y] -> Ok (Point2D (s, x, y)) | _ -> Error "Cound not parse Point2D")
  | 0x59 when length = 4 -> parse_fields length data
    (function [s; x; y; z] -> Ok (Point3D (s, x, y, z)) | _ -> Error "Cound not parse Point3D")
  | _ -> Error "Unknown struct", data

and parse_fields length (data:Bitstring.t) fn =
  let open Tuple2 in
  let internal = fun (l, bitstring) _ ->
    map_fst (parse_one bitstring) ~f:(cons l)
  in
  let fake_list = List.init length ~f:Fn.id in
  List.fold fake_list ~init:([], data) ~f:internal
  |> map_fst ~f:List.rev
  |> map_fst ~f:Result.all
  |> map_fst ~f:(Result.bind ~f:fn)

let parse (bitstring:Bitstring.t) =
  Tuple2.get1 @@ parse_one bitstring

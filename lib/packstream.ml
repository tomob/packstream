open Core

type 'a alist = (string * 'a) list
[@@deriving show]

module rec Message : sig
  type t =
    | Null
    | True
    | False
    | Int of int64
    | Float of float
    | Bytes of bytes
    | String of string
    | List of t list
    | Dict of t alist
    | Struct of {length: int; tag: int; fields: t list}
    | Node of Node.t
    | Relationship of Relationship.t
    | UnboundRelationship of UnboundRelationship.t
    | Path of Path.t
    | Date of {days: int64}
    | Time of {nanoseconds: int64; tz_offset_seconds: int64}
    | LocalTime of {nanoseconds: int64}
    | DateTime of {seconds: int64; nanoseconds: int64; tz_offset_seconds: int64}
    | DateTimeZoneId of {seconds: int64; nanoseconds: int64; tz_id: string}
    | LocalDateTime of {seconds: int64; nanoseconds: int64}
    | Duration of {months: int64; days: int64; seconds: int64; nanoseconds: int64}
    | Point2D of {srid: int64; x: float; y: float}
    | Point3D of {srid: int64; x: float; y: float; z: float}
  [@@deriving show]

  val get_string : t -> (string, string) result
  val get_node : t -> (Node.t, string) result
  val get_relationship : t -> (Relationship.t, string) result
  val get_int : t -> (int64, string) result
  val get_unboundrelationship : t -> (UnboundRelationship.t, string) result

end = struct
  type t =
    | Null
    | True
    | False
    | Int of int64
    | Float of float
    | Bytes of bytes
    | String of string
    | List of t list
    | Dict of t alist
    | Struct of {length: int; tag: int; fields: t list}
    | Node of Node.t
    | Relationship of Relationship.t
    | UnboundRelationship of UnboundRelationship.t
    | Path of Path.t
    | Date of {days: int64}
    | Time of {nanoseconds: int64; tz_offset_seconds: int64}
    | LocalTime of {nanoseconds: int64}
    | DateTime of {seconds: int64; nanoseconds: int64; tz_offset_seconds: int64}
    | DateTimeZoneId of {seconds: int64; nanoseconds: int64; tz_id: string}
    | LocalDateTime of {seconds: int64; nanoseconds: int64}
    | Duration of {months: int64; days: int64; seconds: int64; nanoseconds: int64}
    | Point2D of {srid: int64; x: float; y: float}
    | Point3D of {srid: int64; x: float; y: float; z:float}
  [@@deriving show]

  let get_string = function
    | String s -> Ok s
    | _ -> Error "Could not get string"

  let get_node = function
    | Node n -> Ok n
    | _ -> Error "Could not get string"

  let get_relationship = function
    | Relationship r -> Ok r
    | _ -> Error "Could not get string"

  let get_int = function
    | Int i -> Ok i
    | _ -> Error "Could not get string"

  let get_unboundrelationship = function
    | UnboundRelationship r -> Ok r
    | _ -> Error "Could not get string"

end

and Node : sig
  type t = {id: int64; labels: string list; properties: Message.t alist}
  [@@deriving show]
end = struct
  type t = {id: int64; labels: string list; properties: Message.t alist}
  [@@deriving show]
end

and Relationship : sig
  type t = {id: int64; start_node_id: int64; end_node_id: int64; typ: string; properties: Message.t alist}
  [@@deriving show]
end = struct
  type t = {id: int64; start_node_id: int64; end_node_id: int64; typ: string; properties: Message.t alist}
  [@@deriving show]
end

and UnboundRelationship : sig
  type t = {id: int64; typ: string; properties: Message.t alist}
  [@@deriving show]
end = struct
  type t = {id: int64; typ: string; properties: Message.t alist}
  [@@deriving show]
end

and Path : sig
  type t = {nodes: Node.t list; rels: UnboundRelationship.t list; ids: int64 list}
  [@@deriving show]
end= struct
  type t = {nodes: Node.t list; rels: UnboundRelationship.t list; ids: int64 list}
  [@@deriving show]
end

(* Unwraps strings on the list.
   [String x] -> [x] *)
let unwrap_string_list l fn =
  List.map ~f:Message.get_string l
  |> Result.all
  |> Result.map ~f:fn

let unwrap_list l getter fn =
    List.map ~f:getter l
    |> Result.all
    |> Result.map ~f:fn

let cons lst elem =
  List.cons elem lst

let rec parse_one (bitstring:Bitstring.t) =
  let open Message in
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
  | {| 0xCC : 8; length :  8; bytes : length*8 : string; rest : -1 : bitstring |} ->
    Ok (Bytes (Bytes.of_string bytes)), rest
  | {| 0xCD : 8; length : 16; bytes : length*8 : string; rest : -1 : bitstring |} ->
    Ok (Bytes (Bytes.of_string bytes)), rest
  | {| 0xCE : 8; length : 32; bytes : (Int32.to_int_exn length)*8 : string; rest : -1 : bitstring |} ->
    Ok (Bytes (Bytes.of_string bytes)), rest
  (* Strings *)
  | {| 0x8  : 4; length :  4 : unsigned; str : length*8 : string; rest : -1 : bitstring |} -> Ok (String str), rest
  | {| 0xD0 : 8; length :  8 : unsigned; str : length*8 : string; rest : -1 : bitstring |} -> Ok (String str), rest
  | {| 0xD1 : 8; length : 16 : unsigned; str : length*8 : string; rest : -1 : bitstring |} -> Ok (String str), rest
  | {| 0xD2 : 8; length : 32 : unsigned; str : (Int32.to_int_exn length)*8 : string; rest : -1 : bitstring |} ->
    Ok (String str), rest
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
  |> map_fst ~f:(Result.map ~f:(fun x -> Message.List x))

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
  |> map_fst ~f:(Result.map ~f:(fun x -> Message.Dict x))

and parse_structs length tag (data:Bitstring.t) =
  let open Message in
  match tag with
  | 0x4E when length = 3 -> parse_fields length data
    (function [Int i; List l; Dict p] ->
       unwrap_list l get_string @@ fun lst -> Node {id = i; labels = lst; properties = p}
     | _ -> Error "Cound not parse Node")
  | 0x52 when length = 5 -> parse_fields length data
    (function [Int i; Int s; Int e; String t; Dict p] ->
      Ok (Relationship {id = i; start_node_id = s; end_node_id = e; typ = t; properties = p})
     | _ -> Error "Cound not parse Relationship")
  | 0x72 when length = 3 -> parse_fields length data
    (function [Int i; String t; Dict p] ->
      Ok (UnboundRelationship {id = i; typ = t; properties = p})
     | _ -> Error "Cound not parse UnboundRelationship")
  | 0x50 when length = 3 -> parse_fields length data
    (function [List n; List r; List p] ->
       unwrap_list n get_node (
         fun nodes -> unwrap_list r get_unboundrelationship (
           fun relationships -> unwrap_list p get_int (
             fun ids -> Path {nodes = nodes; rels = relationships; ids = ids}
           )
         ) |> Result.join
       ) |> Result.join
     | _ -> Error "Cound not parse Path")
  | 0x44 when length = 1 -> parse_fields length data
    (function [Int d] -> Ok (Date {days = d})
     | _ -> Error "Cound not parse Date")
  | 0x54 when length = 2 -> parse_fields length data
    (function [Int n; Int t] -> Ok (Time {nanoseconds = n; tz_offset_seconds = t})
     | _ -> Error "Cound not parse Time")
  | 0x74 when length = 1 -> parse_fields length data
    (function [Int t] -> Ok (LocalTime {nanoseconds = t}) | _ -> Error "Cound not parse LocalTime")
  | 0x46 when length = 3 -> parse_fields length data
    (function [Int s; Int n; Int t] -> Ok (DateTime {seconds = s; nanoseconds = n; tz_offset_seconds = t})
     | _ -> Error "Cound not parse DateTime")
  | 0x66 when length = 3 -> parse_fields length data
    (function [Int s; Int n; String t] -> Ok (DateTimeZoneId {seconds = s; nanoseconds = n; tz_id = t})
     | _ -> Error "Cound not parse DateTimeZoneId")
  | 0x64 when length = 2 -> parse_fields length data
    (function [Int s; Int n] -> Ok (LocalDateTime {seconds = s; nanoseconds = n})
     | _ -> Error "Cound not parse LocalDateTime")
  | 0x45 when length = 4 -> parse_fields length data
    (function [Int m; Int d; Int s; Int n] -> Ok (Duration {months = m; days = d; seconds = s; nanoseconds =n})
     | _ -> Error "Cound not parse Duration")
  | 0x58 when length = 3 -> parse_fields length data
    (function [Int s; Float x; Float y] -> Ok (Point2D {srid = s; x = x; y = y})
     | _ -> Error "Cound not parse Point2D")
  | 0x59 when length = 4 -> parse_fields length data
    (function [Int s; Float x; Float y; Float z] -> Ok (Point3D {srid = s; x = x; y = y; z = z})
     | _ -> Error "Cound not parse Point3D")
  | _ -> parse_fields length data
    (fun lst -> Ok (Struct {length = length; tag = tag; fields = lst}))

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

let serialize_int (i:int64) =
  let open Int64 in
  match i with
  | _ when i < -2147483648L || i > 2147483647L -> let%bitstring b = {| 0xCB : 8; i : 64 |} in b
  | _ when i < -32768L || i > 32767L -> let%bitstring b = {| 0xCA : 8; (Int64.to_int32_exn i) : 32 |} in b
  | _ when i < -128L || i > 127L -> let%bitstring b = {| 0xC9 : 8; (Int64.to_int_exn i) : 16 : signed |} in b
  | _ when i < -16L -> let%bitstring b = {| 0xC8 : 8; (Int64.to_int_exn i) : 8 : signed |} in b
  | _ -> let%bitstring b = {| (Int64.to_int_exn i) : 8 : signed |} in b

let serialize_float f =
  let converted = Int64.bits_of_float f in let%bitstring b = {| 0xC1 : 8; converted : 64 |} in b

let serialize_byte_array ba =
  match String.length ba with
  | l when l < 256 -> let%bitstring b = {| 0xCC : 8; l : 8 : unsigned; ba : 8*l : string |} in b
  | l when l < 65536 -> let%bitstring b = {| 0xCD : 8; l : 16 : unsigned; ba : 8*l : string |} in b
  | l -> let%bitstring b = {| 0xCE : 8; (Int32.of_int_exn l) : 32 : unsigned; ba : 8*l : string |} in b

let serialize_string str =
  match String.length str with
  | l when l > 65535 -> let%bitstring b = {| 0xD2 : 8; (Int32.of_int_exn l) : 32 : unsigned; str : l*8 : string|} in b
  | l when l > 255 -> let%bitstring b = {| 0xD1 : 8; l : 16 : unsigned; str : l*8 : string|} in b
  | l when l > 15 -> let%bitstring b = {| 0xD0 : 8; l : 8 : unsigned; str : l*8 : string|} in b
  | l -> let%bitstring b = {| 0x8 : 4; l : 4 : unsigned; str : l*8 : string |} in b

let rec serialize message =
  let open Message in
  match message with
  | Null -> let%bitstring b = {| 0xC0 : 8 |} in b
  | False -> let%bitstring b = {| 0xC2 : 8 |} in b
  | True -> let%bitstring b = {| 0xC3 : 8 |} in b
  | Int i -> serialize_int i
  | Float f -> serialize_float f
  | Bytes ba -> serialize_byte_array (Bytes.to_string ba)
  | String str -> serialize_string str
  | List lst -> serialize_list lst
  | Dict dct -> serialize_dict dct
  | Struct {length; tag; fields} ->
      let%bitstring header = {| 0xb : 4; length : 4 : unsigned; tag : 8 : unsigned |} in
      Bitstring.concat @@ header::(List.map ~f:serialize fields)
  | Node {id; labels; properties} -> let%bitstring header = {| 0xb34e : 16 |} in
      Bitstring.concat [
        header;
        serialize_int id;
        serialize_list (List.map ~f:(fun s -> String s) labels);
        serialize_dict properties]
  | Relationship {id; start_node_id; end_node_id; typ; properties} -> let%bitstring header = {| 0xb552 : 16 |} in
      Bitstring.concat [
        header;
        serialize_int id;
        serialize_int start_node_id;
        serialize_int end_node_id;
        serialize_string typ;
        serialize_dict properties]
  | UnboundRelationship {id; typ; properties} -> let%bitstring header = {| 0xb372 : 16 |} in
      Bitstring.concat [
        header;
        serialize_int id;
        serialize_string typ;
        serialize_dict properties]
  | Path {nodes; rels; ids} -> let%bitstring header = {| 0xb350 : 16 |} in
      Bitstring.concat [
        header;
        serialize_list @@ List.map ~f:(fun n -> Node n) nodes;
        serialize_list @@ List.map ~f:(fun r -> UnboundRelationship r) rels;
        serialize_list @@ List.map ~f:(fun id -> Int id) ids; ]
  | Date {days} -> let%bitstring header = {| 0xb144 : 16 |} in
      Bitstring.concat [header; serialize_int days]
  | Time {nanoseconds; tz_offset_seconds} -> let%bitstring header = {| 0xb254 : 16 |} in
      Bitstring.concat [header; serialize_int nanoseconds; serialize_int tz_offset_seconds]
  | LocalTime {nanoseconds} -> let%bitstring header = {| 0xb174 : 16 |} in
      Bitstring.concat [header; serialize_int nanoseconds]
  | DateTime {seconds; nanoseconds; tz_offset_seconds} -> let%bitstring header = {| 0xb346 : 16 |} in
      Bitstring.concat [
        header;
        serialize_int seconds;
        serialize_int nanoseconds;
        serialize_int tz_offset_seconds]
  | DateTimeZoneId {seconds; nanoseconds; tz_id} -> let%bitstring header = {| 0xb366 : 16 |} in
      Bitstring.concat [header; serialize_int seconds; serialize_int nanoseconds; serialize_string tz_id]
  | LocalDateTime {seconds; nanoseconds} -> let%bitstring header = {| 0xb264 : 16 |} in
      Bitstring.concat [header; serialize_int seconds; serialize_int nanoseconds]
  | Duration {months; days; seconds; nanoseconds} -> let%bitstring header = {| 0xb445 : 16 |} in
      Bitstring.concat [
        header;
        serialize_int months;
        serialize_int days;
        serialize_int seconds;
        serialize_int nanoseconds]
  | Point2D {srid; x; y} -> let%bitstring header = {| 0xb358 : 16 |} in
      Bitstring.concat [header; serialize_int srid; serialize_float x; serialize_float y]
  | Point3D {srid; x; y; z} -> let%bitstring header = {| 0xb459 : 16 |} in
      Bitstring.concat [header; serialize_int srid; serialize_float x; serialize_float y; serialize_float z]

and serialize_list lst =
  let elems = List.map ~f:serialize lst in
    match List.length elems with
    | l when l > 65535 -> let%bitstring header = {| 0xD6 : 8; (Int32.of_int_exn l) : 32|} in Bitstring.concat (header::elems)
    | l when l > 255 -> let%bitstring header = {| 0xD5 : 8; l : 16 : unsigned|} in Bitstring.concat (header::elems)
    | l when l > 15 -> let%bitstring header = {| 0xD4 : 8; l : 8 : unsigned|} in Bitstring.concat (header::elems)
    | l -> let%bitstring header = {| 0x9 : 4; l : 4|} in Bitstring.concat (header::elems)

and serialize_dict dct =
  let elems = List.map ~f:serialize_elem dct |> List.join in
    match List.length dct with
    | l when l > 65535 -> let%bitstring header = {| 0xDA : 8; (Int32.of_int_exn l) : 32|} in Bitstring.concat (header::elems)
    | l when l > 255 -> let%bitstring header = {| 0xD9 : 8; l : 16 : unsigned|} in Bitstring.concat (header::elems)
    | l when l > 15 -> let%bitstring header = {| 0xD8 : 8; l : 8 : unsigned|} in Bitstring.concat (header::elems)
    | l -> let%bitstring header = {| 0xA : 4; l : 4|} in Bitstring.concat (header::elems)

and serialize_elem (key, value) =
  [serialize_string key; serialize value]

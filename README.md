# OCaml Packstream Library

This library can be used to parse and generate Packstream-formatted data.:w


## How to install

```
ocaml install packstream
```

## How to use

```
(executable
 (name        foo)
  (libraries   (packstream)))
```

## Usage

```
utop [0]: open Packstream;;
utop [1]: parse @@ Bitstring.bitstring_of_string "\xB5\x52\x0B\x02\x03\x85KNOWS\xA1\x84name\x87example";;
- : (Message.t, string) result =
Core.Ok
 (Packstream.Message.Relationship
   {Packstream.Relationship.id = 11; start_node_id = 2; end_node_id = 3;
    typ = "KNOWS";
    properties = [("name", Packstream.Message.String "example")]})

 utop [2]: serialize @@ Struct { length = 3; tag = 0x12; fields = [Int 1L; String "Howdy"; List [Float 1.23; True]] };;
- : Bitstring.t =
("\179\018\001\133Howdy\146\193?\243\174\020z\225G\174\195", 0, 160)
```

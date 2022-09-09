open Angstrom
open Parsers
open Type
open Prelude
open Conf

(*
   Property kinds:
   1. [[a]], [[b]], [[c [[d]]]]
   2. [[a]], #b, #c
   3. "abc"
   4. [[c]] blabla [[b]]
*)

(* steps:
   1. check whether the property value is enclosed by quotes, if so, return empty
   1. parsing all the links
   2. if there's no links, check whether it's separated by `,`
*)

let property_references config s =
  let config = { config with inline_skip_macro = true } in
  let end_quoted = match last_char s with
    | Some '"' -> true
    | _ -> false in
  if s.[0] == '"' && end_quoted then
    []
  else
    match parse_string ~consume:All (Inline.parse config) s with
    | Ok result ->
      let result = List.map fst result in
      List.filter (fun e ->
          match e with
          | Inline.Tag _ -> true
          | Inline.Link _ -> true
          | Inline.Nested_link _ -> true
          | _ -> false
        )
        result
    | Error _ -> []

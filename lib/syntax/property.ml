open Angstrom
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

let keep_refs result =
  List.filter
    (fun e ->
      match e with
      | Inline.Tag _ -> true
      | Inline.Link _ -> true
      | Inline.Nested_link _ -> true
      | _ -> false)
    result

(** Page refs, tags, block refs, macros-with-refs. Autolinks/emphasis are not
    property references. *)
let may_have_property_refs s =
  let n = String.length s in
  let rec loop i =
    if i >= n then
      false
    else
      match s.[i] with
      | '#'
      | '['
      | '{' ->
        true
      | '(' when i + 1 < n && s.[i + 1] = '(' -> true
      | _ -> loop (i + 1)
  in
  loop 0

let parse_refs_inline config s =
  match parse_string ~consume:All (Inline.parse config) s with
  | Ok result -> keep_refs (List.map fst result)
  | Error _ -> []

let property_references config s =
  let config = { config with inline_skip_macro = true } in
  let end_quoted =
    match last_char s with
    | Some '"' -> true
    | _ -> false
  in
  if s = "" || (s.[0] == '"' && end_quoted) then
    []
  else if not (may_have_property_refs s) then
    []
  else
    match Outline_inline.try_fast_scan s with
    | Some result -> keep_refs (List.map fst result)
    | None -> parse_refs_inline config s

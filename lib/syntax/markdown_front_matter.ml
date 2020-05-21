(* https://jekyllrb.com/docs/front-matter/ *)
(*
---
layout: post
title: Blogging Like a Hacker
---
*)

open Angstrom
open Parsers
open Type

let kv_parse =
  let key_parser =
    take_while1 (fun c -> c <> ':' && non_eol c) in
  let sep_parser =
    char ':' <* spaces in
  let value_parser =
    ((take_till is_eol) <|> (end_of_input >>| fun _ -> "")) in
  lift3 (fun key _sep value ->
      [Directive (key, value)])
    key_parser sep_parser value_parser

let parse =
  string "---" *> end_of_line *>
  end_string "---" (fun s ->
      (* multiple directives *)
      match parse_string (many1 (kv_parse <* (end_of_line <|> end_of_input)) ) s with
      | Ok result -> result
      | Error _e -> []
    )

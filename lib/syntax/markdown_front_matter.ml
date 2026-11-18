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
    spaces *> take_while1 (fun c -> c <> ':' && non_eol c) >>= fun raw_key ->
    let key = String.trim raw_key in
    if key = "" then
      fail "empty front-matter key"
    else
      return key
  in
  let sep_parser = char ':' <* spaces in
  let value_parser =
    take_till is_eol >>| String.trim <|> (end_of_input >>| fun _ -> "")
  in
  let p =
    lift3
      (fun key _sep value -> Directive (key, value))
      key_parser sep_parser value_parser
  in
  Helper.with_pos_meta p

let blank_lines = skip_many (spaces *> end_of_line)

let directives_parse =
  let line_end = end_of_line *> blank_lines <|> end_of_input in
  blank_lines *> many1 (kv_parse <* line_end) <* spaces

let parse =
  string "---" *> spaces *> end_of_line
  *> end_string "---" (fun s ->
         (* multiple directives *)
         match parse_string ~consume:All directives_parse s with
         | Ok result -> result
         | Error _e -> [])

open Angstrom
open Parsers
open Prelude

type t =
  { level: int
  ; marker: string option
  ; priority: char option
  ; title: string
  ; tags: string list }

(* TODO, DOING, DONE *)
let marker = string "TODO" <|> string "DOING" <|> string "DONE"

let level = many1 @@ char '*'

let priority = string "[#" *> any_char <* char ']'

let seperated_tags = sep_by (char ':') (take_while (fun x -> x <> ':'))

let tags = char ':' *> seperated_tags

(* not priority, tags, marker *)
let title = take_while (function ':' | '[' | '\r' | '\n' -> false | _ -> true)

let is_blank s =
  let n = String.length s in
  let rec aut_is_blank i =
    if i = n then true
    else
      let c = s.[i] in
      if is_space c then aut_is_blank (i + 1) else false
  in
  aut_is_blank 0

let parse =
  lift5
    (fun level marker priority title tags ->
      let level = List.length level in
      let tags = remove is_blank tags in
      {level; marker; priority; title; tags} )
    (spaces *> level <* spaces <?> "Heading level")
    (optional (lex marker <?> "Heading marker"))
    (optional (lex priority <?> "Heading priority"))
    (lex title <?> "Heading title")
    (optional_list (lex tags <?> "Heading tags"))

(*

let _ =
  let input = "* TODO [#A] hello :foo:bar:" in
  parse_string heading input

*)

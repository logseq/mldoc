open Angstrom
open Parsers
open Prelude
open Org

(* TODO, DOING, DONE *)
let marker = string "TODO" <|> string "DOING" <|> string "DONE"

let level = take_while1 (fun c -> c = '*')

let priority = string "[#" *> any_char <* char ']'

let seperated_tags = sep_by (char ':') (take_while1 (fun x -> x <> ':'))

let tags = char ':' *> seperated_tags

(* not priority, tags, marker *)
let title = take_while1 (function ':' | '[' | '\r' | '\n' -> false | _ -> true)

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
      let level = String.length level in
      let tags = remove is_blank tags in
      let title = match (parse_string Inline.parse title) with
        | Ok title -> title
        | Error e -> [] in
      Heading {level; marker; priority; title; tags} )
    (level <* ws <?> "Heading level")
    (optional (lex marker <?> "Heading marker"))
    (optional (lex priority <?> "Heading priority"))
    (lex title <?> "Heading title")
    (optional_list (lex tags <?> "Heading tags"))

(*

let _ =
  let input = "* TODO [#A] hello :foo:bar:" in
  parse_string heading input

*)

open Angstrom
open Parsers
open Prelude
open Type
open Conf

(* TODO: Markdown alternate syntax,
   https://www.markdownguide.org/basic-syntax/#alternate-syntax
*)

(* todo keywords *)
let marker =
  string "TODO" <|> string "DOING" <|> string "WAITING"
  <|> string "WAIT" <|> string "DONE" <|> string "CANCELED" <|> string "CANCELLED"
  <|> string "STARTED" <|> string "IN-PROGRESS"
  <|> string "NOW" <|> string "LATER"
  >>= fun s ->
  peek_char >>= function
  | None -> return s
  | Some c ->
    if c == ' ' then
      return s
    else
      fail "Marker should followed by some spaces"

let org_level = take_while1 (fun c -> c = '*')

let level config =
  match config.format with
  | Org -> org_level
  | Markdown -> Markdown_level.parse

let priority = string "[#" *> any_char <* char ']'

let seperated_tags = sep_by (char ':') (take_while1 (fun x -> x <> ':' && non_space_eol x))

let tags = char ':' *> seperated_tags <* char ':'

(* not priority, tags, marker *)
let title = take_while (function | '\r' | '\n' -> false | _ -> true)

let is_blank s =
  let n = String.length s in
  let rec aut_is_blank i =
    if i = n then true
    else
      let c = s.[i] in
      if is_space c then aut_is_blank (i + 1) else false
  in
  aut_is_blank 0

let anchor_link s =
  let map_char = function
    | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '(' | ')') as c -> String.make 1 c
    | ' ' | '_' | '-' -> "_"
    | c -> Printf.sprintf "-%x-" (int_of_char c)
  in
  Prelude.explode (String.trim s) |> CCList.map map_char |> String.concat ""

let parse config =
  let p = lift4
      (fun level marker priority title ->
         let level = String.length level in
         let title = match title with
           | None -> []
           | Some title ->
             match (parse_string ~consume:All (Inline.parse config) (String.trim title)) with
             | Ok title -> title
             | Error _e -> [] in
         let (title, tags) = match title with
           | [] -> (title, [])
           | _ ->
             let last_inline = List.nth title (List.length title - 1) in
             match last_inline with
             | Inline.Plain s ->
               let s = String.trim s in
               if String.length s > 1 && s.[String.length s - 1] = ':' then
                 let (prefix, maybe_tags) = (splitr (fun c -> c <> ' ') s) in
                 (match parse_string ~consume:All tags maybe_tags with
                  | Ok tags ->
                    let title = if prefix = "" then (drop_last 1 title) else
                        (drop_last 1 title) @ [Inline.Plain prefix] in
                    (title, remove is_blank tags)
                  | _ ->
                    (title, []))
               else
                 (title, [])

             | _ -> (title, []) in
         let anchor = anchor_link (Inline.asciis title) in
         let meta = { timestamps = []; properties = []} in
         Heading {level; marker; priority; title; tags; anchor; meta; numbering=None} )
      (level config <?> "Heading level")
      (optional (ws *> marker <?> "Heading marker"))
      (optional (ws *> priority <?> "Heading priority"))
      (optional (ws *> title <?> "Heading title")) in
  p <* (end_of_line <|> end_of_input)

(*

let _ =
  let input = "* TODO [#A] hello :foo:bar:" in
  parse_string ~consume:All heading input

*)

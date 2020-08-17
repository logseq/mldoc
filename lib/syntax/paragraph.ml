open Angstrom
open Parsers
open Type
open Conf
open Prelude

(* inline and footnotes *)

let sep =
  eols >>= fun s ->
  if String.length s >= 2 then
    return Paragraph_Sep
  else
    fail "only 1 eol"

let trim_last_space s =
  let n = String.length s in
  if n > 0 && s.[n-1] = ' ' then
    String.sub s 0 (n-1)
  else
    s

let parse =
  (line <* optional eols) >>| fun l -> Paragraph_line l

let concat_paragraph_lines config l =
  let l = List.append l [Horizontal_Rule] in
  let (l, _) = List.fold_left (fun (acc, lines) item ->
      match item with
      | Paragraph_line line ->
        (acc, line :: lines)
      | other ->
        if List.length lines > 0 then
          let lines = List.rev lines in
          let content = (String.concat "\n" lines) in
          let paragraph = match parse_string (Inline.parse config) content with
            | Ok result -> Paragraph result
            | Error _ -> Paragraph [Inline.Plain content] in
          let acc = other :: paragraph :: acc in
          (acc, [])
        else
          (other :: acc, [])
    ) ([], []) l in
  let l = List.rev l in
  drop_last 1 l

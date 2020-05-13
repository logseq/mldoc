open Angstrom
open Parsers
open Type

(* https://www.markdownguide.org/basic-syntax/#code-blocks *)
(*
Code Blocks
To create code blocks, indent every line of the block by at least four spaces or one tab.

    <html>
      <head>
      </head>
    </html>
*)
let single_line =
  let spaces = ws >>= fun s ->
    let indent = String.length s in
    if indent >= 4 then return indent else fail "single_line not enough spaces"
  in
  lift2 (fun indent content -> (indent, content)) spaces line

let parse = many1 (single_line <* (end_of_line <|> end_of_input) <* optional eols)
  >>= fun lines ->
  let start_indent, _content = List.hd lines in
  let lines = List.map (fun (i, c) ->
      if i > start_indent then
        (String.make (i - start_indent) ' ') ^ c
      else
        c
    ) lines in
  return [Example lines]

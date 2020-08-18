open Angstrom
open Parsers
open Type
open Conf
open Prelude

(* inline and footnotes *)

let sep =
  take_while1 is_eol >>| fun _ -> Paragraph_Sep

let trim_last_space s =
  let n = String.length s in
  if n > 0 && s.[n-1] = ' ' then
    String.sub s 0 (n-1)
  else
    s

let parse =
  (line <* optional eols) >>| fun l -> Paragraph_line l

let concat_paragraph_lines config l =
  let l = List.append l [(Horizontal_Rule, {start_pos = 0; end_pos = 0})] in
  let (l, _, _, _) = List.fold_left (fun (acc, lines, pos1, pos2) item ->
      match item with
      | (Paragraph_line line, {start_pos; end_pos}) ->
        let start_pos = if pos1 = 0 then start_pos else pos1 in
        (acc, line :: lines, start_pos, end_pos)
      | (other, pos_meta) ->
        if List.length lines > 0 then
          let lines = List.rev lines in
          let content = (String.concat "\n" lines) in
          let paragraph = match parse_string (Inline.parse config) content with
            | Ok result -> Paragraph result
            | Error _ -> Paragraph [Inline.Plain content] in
          let paragraph_with_meta = (paragraph, {start_pos = pos1; end_pos = pos2}) in
          let acc = item :: paragraph_with_meta :: acc in
          (acc, [], 0, 0)
        else
          (item :: acc, [], 0, 0)
    ) ([], [], 0, 0) l in
  let l = List.rev l in
  drop_last 1 l

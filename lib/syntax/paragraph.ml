open Angstrom
open Parsers
open Type
open Conf
open Prelude

(* inline and footnotes *)

let sep =
  take_while1 is_eol >>| fun s -> Paragraph_Sep (String.length s)

let trim_last_space s =
  let n = String.length s in
  if n > 0 && s.[n-1] = ' ' then
    String.sub s 0 (n-1)
  else
    s

let parse =
  line >>| fun l -> Paragraph_line l

let parse_lines config lines pos1 pos2 =
  let lines = List.rev lines in
  let content = (String.concat "" lines) in
  let paragraph = match parse_string (Inline.parse config) content with
    | Ok result -> Paragraph result
    | Error _ -> Paragraph [Inline.Plain content] in
  (paragraph, {start_pos = pos1; end_pos = pos2})

let concat_paragraph_lines config l =
  let (acc, lines, pos1, pos2) = List.fold_left (fun (acc, lines, pos1, pos2) item ->
      match item with
      | (Paragraph_line line, {start_pos; end_pos}) ->
        let start_pos = if pos1 = 0 then start_pos else pos1 in
        (acc, line :: lines, start_pos, end_pos)
      | (Paragraph_Sep n, {start_pos; end_pos}) ->
        let line = repeat n "\n" in
        let line = String.concat "" line in
        (acc, line :: lines, start_pos, end_pos)
      | (_other, _pos_meta) ->
        if List.length lines > 0 then
          let paragraph_with_meta = parse_lines config lines pos1 pos2 in
          let acc = item :: paragraph_with_meta :: acc in
          (acc, [], 0, 0)
        else
          (item :: acc, [], 0, 0)
    ) ([], [], 0, 0) l in
  let acc = if List.length lines > 0 then
      (* FIXME: wrong meta *)
      let paragraph_with_meta = parse_lines config lines pos1 pos2 in
      paragraph_with_meta :: acc
    else
      acc in
  List.rev acc

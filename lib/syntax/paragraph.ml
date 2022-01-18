open! Prelude
open Angstrom
open Parsers
open Type
open Pos
open Conf

(* inline and footnotes *)

let sep = take_while1 is_eol >>| fun s -> Paragraph_Sep (String.length s)

let trim_last_space s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = ' ' then
    String.sub s 0 (n - 1)
  else
    s

let parse = line >>| fun l -> Paragraph_line l

let parse_lines config lines pos1 pos2 =
  let lines = List.rev lines in
  let content = String.concat "" lines in
  let paragraph =
    let inline_parse = if config.parse_outline_only then Outline_inline.parse else Inline.parse in
    match parse_string ~consume:All (inline_parse config) content with
    | Ok result -> Paragraph result
    | Error _ ->
      Paragraph (Type_op.inline_list_with_none_pos [ Inline.Plain content ])
  in
  (paragraph, { start_pos = pos1; end_pos = pos2 })

let concat_paragraph_lines config l =
  let acc, lines, pos1, pos2 =
    List.fold_left
      (fun (acc, lines, pos1, pos2) item ->
        match item with
        | Paragraph_line line, { start_pos; end_pos } ->
          let start_pos =
            if Option.is_none pos1 then
              Some start_pos
            else
              pos1
          in
          (acc, line :: lines, start_pos, end_pos)
        | Paragraph_Sep n, { start_pos; end_pos } ->
          let line = repeat n "\n" in
          let line = String.concat "" line in
          let start_pos =
            if Option.is_none pos1 then
              Some start_pos
            else
              pos1
          in
          (acc, line :: lines, start_pos, end_pos)
        | _other, _pos_meta ->
          if List.length lines > 0 then
            let pos1 = Option.default 0 pos1 in
            let paragraph_with_meta = parse_lines config lines pos1 pos2 in
            let acc = item :: paragraph_with_meta :: acc in
            (acc, [], None, 0)
          else
            (item :: acc, [], None, 0))
      ([], [], None, 0) l
  in
  let acc =
    if List.length lines > 0 then
      let pos1 = Option.default 0 pos1 in
      let paragraph_with_meta = parse_lines config lines pos1 pos2 in
      paragraph_with_meta :: acc
    else
      acc
  in
  List.rev acc

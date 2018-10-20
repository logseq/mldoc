open Angstrom
open Parsers
open Prelude
(*
|--------------------------------+---------------------|
| (1, 1)                         | (2, 1) and /inline/ |
| Second line, first group       | =stuff=             |
|--------------------------------+---------------------|
| First line of the second group | *bold*              |
|--------------------------------+---------------------|

*)

(* A table can has multiple groups, and each group can has multiple rows. The first group is the table header.*)

let separated_line =
  optional ws *> char '|' *> many1 (char '-' <|> char '+') *> char '|' *> optional eol

let split_into_columns s =
  String.split_on_char '|' s
  |> List.map String.trim

let row_line =
  let open String in
  optional ws *> char '|' *>
  (peek_char_fail >>| fun c -> c <> '-') (* ensure not begin with "|-" *)
  *> take_till is_eol
  >>= fun line ->
  let line = trim line in
  let len = (length line - 1) in
  if get line len <> '|' then
    fail "raw_line"
  else
    let s = sub line 0 len in
    return @@ split_into_columns s

let group =
  let p rows =
    fix (fun p ->
        row_line >>= fun row ->
        print_list row;
        rows := row :: ! rows;
        p
        <|>
        return !rows) in
  clear_parser_resource p (ref []) "table group"

let table =
    let p groups =
    fix (fun p ->
        group >>= fun g ->
        groups := g :: ! groups;
        p
        <|>
        return !groups) in
  clear_parser_resource p (ref []) "table"

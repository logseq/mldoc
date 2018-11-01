open Angstrom
open Parsers
open Prelude
open Org

(*
|Foo                             | Bar                 |
|--------------------------------+---------------------|
| (1, 1)                         | (2, 1) and /inline/ |
| Second line, first group       | =stuff=             |
|--------------------------------+---------------------|
| First line of the second group | *bold*              |
|--------------------------------+---------------------|

*)

(* A table can has multiple groups, and each group can has multiple rows. The first row is the table header.*)

let separated_line =
  optional ws *> char '|' *> take_while1 (fun c -> c = '-' || c = '+') *> char '|' *> optional eol

let split_into_columns s =
  String.split_on_char '|' s
  |> List.map String.trim

let row_line separated =
  let open String in
  optional ws *> char '|' *>
  (peek_char_fail >>| fun c ->
   let sep = c = '-' in
   separated := sep;
   not sep)
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
    let separated = ref false in
    fix (fun p ->
        row_line separated >>= fun row ->
        if ! separated then
          return @@ List.rev !rows
        else
          let row = List.map (fun col ->
              result_default [Inline.Plain col] (parse_string Inline.parse col)) row in
          (rows := row :: ! rows;
           p)
          <|>
          return @@ List.rev !rows
      ) in
  clear_parser_resource p (ref []) "table group"

let parse =
  let p groups =
    fix (fun p ->
        group >>= fun g ->
        groups := g :: ! groups;
        p
        <|>
        return @@ List.rev !groups) in
  optional eols *>
  optional separated_line *>
  clear_parser_resource p (ref []) "table"
  >>| function
  | [] -> Table { header = None;
                  groups = []}
  | [] :: t ->
    Table { header = None;
            groups = t }
  | (h1 :: t1) :: t ->
    let groups = if List.length t1 = 0 then t
      else List.concat [[t1]; t] in
    Table {header = Some h1;
           groups}

open Angstrom
open Parsers
open Prelude
open Org

(* TODO: support column groups *)

(*
| Foo                            | Bar                 |
|--------------------------------+---------------------|
| (1, 1)                         | (2, 1) and /inline/ |
| Second line, first group       | =stuff=             |
|--------------------------------+---------------------|
| First line of the second group | *bold*              |
|--------------------------------+---------------------|
#+TBLFM: $2=$1^2::$3=$1^3::$4=$1^4::$5=sqrt($1)::$6=sqrt(sqrt(($1)))

*)

(* A table can has multiple groups, and each group can has multiple rows. The first row is the table header.*)

let boundaries_spec =
  optional ws *> string "#+TBLFM:" *> line

let separated_line =
  optional ws *> char '|' *> take_while1 (fun c -> c = '-' || c = '+') *> char '|' *> optional ws *> optional eol

let split_into_columns s =
  String.split_on_char '|' s
  |> List.map String.trim

let row_line =
  let open String in
  optional ws *> char '|' *> take_till is_eol <* optional eol
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
        optional separated_line >>= function
        | None ->               (* new row *)
          row_line >>= fun row ->
          let row = List.map (fun col ->
              result_default [Inline.Plain col] (parse_string Inline.parse col)) row in
          rows := row :: !rows;
          p <|> return @@ List.rev !rows
        | Some _ ->             (* separated *)
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
  clear_parser_resource p (ref []) "table"
  <* optional boundaries_spec
  >>| function
  | [] ->
    [Table { header = None;
             groups = []}]
  | [] :: t ->
    [Table { header = None;
             groups = t }]
  | (h1 :: t1) :: t ->
    let groups = if List.length t1 = 0 then t
      else List.concat [[t1]; t] in
    [Table {header = Some h1;
            groups}]

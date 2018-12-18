open Angstrom
open Parsers
open Prelude
open Type

let boundaries_spec =
  spaces *> string "#+TBLFM:" *> line

let separated_line =
  spaces *> char '|' *> take_while1 (fun c -> c = '-' || c = '+') *> char '|' *> spaces *> optional eol

let split_into_columns s =
  String.split_on_char '|' s
  |> List.map String.trim

let row_line =
  let open String in
  spaces *> char '|' *> take_till1 is_eol <* optional eol
  >>= fun line ->
  let line = trim line in
  let len = (length line - 1) in
  if len >= 0 then
    if get line len <> '|' then
      fail "raw_line"
    else
      let s = sub line 0 len in
      return @@ split_into_columns s
  else
    fail "raw_line"

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

let is_col_row row =
  let open Inline in
  List.for_all
    (function
      | [Plain s] -> s = "/" || s = "<" || s = "" || s = ">"
      | _ -> false)
    row

let build_col_groups row =
  let open Inline in
  let open List in
  try
    let l = fold_left
        (fun acc element ->
           match element with
           | [Plain "/"] -> 1 :: acc
           | [Plain "<"] -> 1 :: acc
           | [Plain ""] | [Plain ">"] ->
             (hd acc + 1) :: (tl acc)
           | _ -> failwith "build_col_groups"
        )
        []
        row in
    (rev l)
  with _ -> [(length row)]

let extract_col_row header t =
  let open List in
  try
    match hd (hd t) with
    | row when is_col_row row ->
      (header, (tl (hd t)) :: (tl t), row)
    | row -> (header, t, row)
  with _ -> (header, t, [])

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
  >>= function groups ->
    let (header, groups, col_groups) = match groups with
      | [] ->
        (None, [], [])
      | [] :: t ->
        extract_col_row None t
      | (h1 :: t1) :: t ->
        let groups = if List.length t1 = 0 then t
          else List.concat [[t1]; t] in
        extract_col_row (Some h1) groups
    in
    let col_groups = build_col_groups col_groups in
    return [Table {header; groups; col_groups}]

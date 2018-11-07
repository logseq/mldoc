open Angstrom
open Parsers
open Org

(* inline and footnotes *)

let parse_paragraph interrupt_parsers lines =
  let open List in
  let join_lines lines =
    let result = rev !lines in
    concat (Prelude.join [Inline.Break_Line] result)
  in
  fix (fun parse ->
      Inline.parse <* optional eol >>= fun line ->
      let _ = lines := line :: !lines in
      (choice interrupt_parsers >>= fun blocks ->
       return [Paragraph (join_lines lines); hd blocks])
      <|>
      parse
      <|>
      return [Paragraph (join_lines lines)])

(* TODO: definition support multiple lines *)
let footnote_reference =
  let name_part =
    string "[fn:" *> take_while1 (fun c -> c <> ']' && non_eol c)
    <* char ']' <* optional spaces in
  let definition_part = line in
  lift2
    (fun name definition ->
       let definition = Inline.footnote_inline_definition definition in
       Org.Footnote_Definition (name, definition))
    name_part definition_part

let parse interrupt_parsers =
  let lines = ref [] in
  let p = parse_paragraph interrupt_parsers lines in
  optional eols *>
  (* check for footer reference first *)
  peek_char_fail >>= (function
      | '[' -> (footnote_reference >>| fun f -> [f])
               <|> p
      | _ -> p)
  >>= fun result ->
  let _ = lines := [] in
  return result
  <|>
  let _ = lines := [] in
  fail "paragraph parse"
(*
parse_string (parse Org_parser.interrupt_parsers) "hello world"
*)

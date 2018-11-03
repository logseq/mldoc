open Angstrom
open Parsers
open Types

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

let lines = ref []
let parse interrupt_parsers =
  optional eols *>
  parse_paragraph interrupt_parsers lines >>= fun result ->
  let _ = lines := [] in
  return result
(*
parse_string (parse Org_parser.interrupt_parsers) "hello world"
*)

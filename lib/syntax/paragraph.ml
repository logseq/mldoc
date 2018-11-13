open Angstrom
open Parsers
open Type

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

(* https://orgmode.org/manual/Footnotes.html *)
(* It ends at the next footnote definition, headline, or after two consecutive empty lines. *)
let footnote_definition lines =
  fix (fun footnote_definition ->
      line <* optional eol >>= fun line ->
      lines := line :: !lines;
      two_eols (List.rev !lines)
      <|>
      (peek_string 4 >>= function
        | "[fn:" ->
          return (List.rev !lines)
        | s when s.[0] = '*' && (List.for_all (fun c -> c = '*' || is_space c) (Prelude.explode s)) ->
          return (List.rev !lines)
        | _ ->
          footnote_definition)
      <|>
      return (List.rev !lines))

let footnote_reference lines =
  let name_part =
    string "[fn:" *> take_while1 (fun c -> c <> ']' && non_eol c)
    <* char ']' <* optional spaces in
  let definition_part = footnote_definition lines in
  lift2
    (fun name definition ->
       let definition = Inline.footnote_inline_definition ~break:true (String.concat "\n" definition) in
       Footnote_Definition (name, definition))
    name_part definition_part

let parse interrupt_parsers =
  let lines = ref [] in
  let p = parse_paragraph interrupt_parsers lines in
  optional eols *>
  (* check for footer reference first *)
  peek_char_fail >>= (function
      | '[' ->
        let footnote_definition_lines = ref [] in
        (footnote_reference footnote_definition_lines >>| fun f -> [f])
        <|> p
      | _ -> p)
  >>= fun result ->
  let _ = lines := [] in
  return result
  <|>
  let _ = lines := [] in
  fail "paragraph parse"

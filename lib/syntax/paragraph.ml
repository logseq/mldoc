open Angstrom
open Parsers
open Type
open Config

(* inline and footnotes *)

let parse_paragraph config interrupt_parsers lines =
  let open List in
  let inline_parse () =
    let lines = List.map (fun (s, b) -> if b then s else s ^ " ") (rev !lines) in
    let content = String.concat "" lines in
    let content = String.trim content in
    match parse_string Inline.parse content with
    | Ok result -> Paragraph result
    | Error _e -> Paragraph [Inline.Plain content] in
  fix (fun parse ->
      Inline.break_or_line >>= fun line ->
      let hard_break_inline = "\\\n" in
      let () = match line with
        | Inline.Plain s ->
          let hard_break = Prelude.ends_with s "\\" in
          let item = if hard_break then
              (String.sub s 0 (String.length s - 2), false)
            else (s, false) in
          lines := item :: !lines;
          if hard_break then
            lines := (hard_break_inline, true) :: !lines
        | Inline.Hard_Break_Line ->
          lines := (hard_break_inline, true) :: !lines
        | Inline.Break_Line ->
          if config.keep_line_break then
            lines := ("\n", true) :: !lines
          else
            ()
        | _ ->
          () in
      (* parse plain lines *)
      (choice interrupt_parsers >>= fun blocks ->
       return [inline_parse () ; hd blocks])
      <|>
      parse
      <|>
      return [inline_parse ()]
    )

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

let parse config interrupt_parsers =
  let lines = ref [] in
  let p = parse_paragraph config interrupt_parsers lines in
  optional eols *>
  (* check for footer reference first *)
  peek_char_fail >>= fun c ->
  let p = match c with
    | '[' ->
      let footnote_definition_lines = ref [] in
      (footnote_reference footnote_definition_lines >>| fun f -> [f])
      <|> p
    | _ -> p in
  p >>= fun result ->
  let _ = lines := [] in
  return result
  <|>
  let _ = lines := [] in
  fail "paragraph parse"

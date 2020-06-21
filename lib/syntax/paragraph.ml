open Angstrom
open Parsers
open Type
open Conf

(* inline and footnotes *)

let sep =
  eols >>= fun s ->
  if String.length s >= 2 then
    return [Paragraph_Sep]
  else
    fail "only 1 eol"

let trim_last_space s =
  let n = String.length s in
  if n > 0 && s.[n-1] = ' ' then
    String.sub s 0 (n-1)
  else
    s

let parse_paragraph config interrupt_parsers lines =
  let open List in
  let inline_parse () =
    let lines = List.map (fun (s, b) -> if b then s else s ^ " ") (rev !lines) in
    let content = String.concat "" lines in
    let content = trim_last_space content in
    match parse_string (Inline.parse config) content with
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

let parse config interrupt_parsers =
  let lines = ref [] in
  let p = parse_paragraph config interrupt_parsers lines in
  optional eols *>
  p >>= fun result ->
  let _ = lines := [] in
  return result
  <|>
  let _ = lines := [] in
  fail "paragraph parse"

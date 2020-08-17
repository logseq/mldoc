open Angstrom
open Type
open Prelude

let list_content_parsers config =
  many1 (choice [
      Table.parse config
    ; Block.parse config
    ; Latex_env.parse config
    ; Hr.parse config
    ; Block.results
    ; Comment.parse config
    ; Paragraph.parse
    ])

(* Orders care *)
let parsers config =
  [ Directive.parse
  ; Heading.parse config
  ; Paragraph.sep
  ; Table.parse config
  ; Latex_env.parse config
  ; Drawer.parse
  ; Lists.parse config (list_content_parsers config)
  ; Block.parse config
  ; Hr.parse config
  ; Block.results
  ; Footnote.parse config
  ; Comment.parse config
  ; Paragraph.parse
  ]

let with_pos_meta p =
  lift3 (fun start_pos t end_pos ->
      (t, {start_pos; end_pos}))
    pos p pos

let md_front_matter_parse parse =
  (* with_pos_meta Markdown_front_matter.parse >>= *)
  Markdown_front_matter.parse >>=
  fun fm_result ->
  parse >>= fun result ->
  return (List.append fm_result result)

let parsers config =
  let parsers = parsers config in
  let choices = choice parsers
  in
  let parse = many choices in
  md_front_matter_parse parse
  <|> parse

let parse config input =
  match parse_string (parsers config) input with
  | Ok result ->
    Paragraph.concat_paragraph_lines config result
  | Error err -> failwith err

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

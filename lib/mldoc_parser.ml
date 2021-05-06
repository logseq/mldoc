open Angstrom
open! Prelude

let list_content_parsers config =
  let p =
    choice
      [ Table.parse config
      ; Block.parse config
      ; Latex_env.parse config
      ; Hr.parse config
      ; Block.results
      ; Comment.parse config
      ; Paragraph.parse
      ; Paragraph.sep
      ]
  in
  let p = Helper.with_pos_meta p in
  many1 p

(* Orders care *)
let parsers config =
  [ Directive.parse
  ; Drawer.parse config
  ; Heading.parse config
  ; Paragraph.sep
  ; Table.parse config
  ; Latex_env.parse config
  ; Lists.parse config (list_content_parsers config)
  ; Block.parse config
  ; Hr.parse config
  ; Block.results
  ; Footnote.parse config
  ; Comment.parse config
  ; Paragraph.parse
  ]

let md_front_matter_parse parse =
  Markdown_front_matter.parse >>= fun fm_result ->
  parse >>= fun result -> return (List.append fm_result result)

let parsers config =
  let parsers = parsers config in
  let choice = choice parsers in
  let p = Helper.with_pos_meta choice in
  let parse = many p in
  md_front_matter_parse parse <|> parse

let parse config input =
  match parse_string ~consume:All (parsers config) input with
  | Ok result ->
    Paragraph.concat_paragraph_lines config result |> Reset_level.reset
  | Error err -> failwith err

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

open Angstrom
open! Prelude

let list_content_parsers config =
  let p =
    choice
      [ Table.parse config
      ; Type_parser.Block.parse config
      ; Latex_env.parse config
      ; Hr.parse config
      ; Type_parser.Block.results
      ; Comment.parse config
      ; Paragraph.parse
      ; Paragraph.sep
      ]
  in
  let p = Helper.with_pos_meta p in
  many1 p

(* Orders care *)
let parsers config =
  [ Paragraph.sep
  ; Directive.parse
  ; Drawer.parse config
  ; Type_parser.Heading.parse config
  ; Table.parse config
  ; Latex_env.parse config
  ; Type_parser.Lists.parse config (list_content_parsers config)
  ; Type_parser.Block.parse config
  ; Hr.parse config
  ; Type_parser.Block.results
  ; Footnote.parse config
  ; Comment.parse config
  ; Paragraph.parse
  ]

(* TODO: ignore tags, page/block refs from Src, Example, etc. *)
let outline_parsers config =
  [ Paragraph.sep
  ; Type_parser.Heading.parse config
  ; Drawer.parse config
  ; Directive.parse
  ; Paragraph.parse
  ]

let md_front_matter_parse parse =
  Markdown_front_matter.parse >>= fun fm_result ->
  parse >>= fun result -> return (List.append fm_result result)

let build_parsers parsers config =
  let parsers = parsers config in
  let choice = choice parsers in
  let p = Helper.with_pos_meta choice in
  let parse = many p in
  md_front_matter_parse parse <|> parse

let parse config input =
  let outline_only = Conf.(config.parse_outline_only) in
  let parsers = build_parsers parsers config in
  match parse_string ~consume:All parsers input with
  | Ok result ->
    let ast = Paragraph.concat_paragraph_lines config result in
    let ast =
      if outline_only then
        Prelude.remove
          (fun (t, _) ->
            match t with
            | Type.Results
            | Type.Example _
            | Type.Src _
            | Type.Latex_Environment _
            | Type.Latex_Fragment _
            | Type.Displayed_Math _
            | Type.Horizontal_Rule
            | Type.Raw_Html _
            | Type.Hiccup _ ->
              true
            | _ -> false)
          ast
      else
        ast
    in
    if Conf.is_markdown config then
      List.map (fun (t, pos) -> (Type_op.md_unescaped t, pos)) ast
    else
      ast
  | Error err -> failwith err

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

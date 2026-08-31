open Angstrom
open! Prelude
open Parsers

let list_content_parsers config =
  let p =
    if config.Conf.parse_outline_only then
      choice
        [ Drawer.parse config
        ; Type_parser.Block.parse config
        ; Paragraph.parse
        ; Paragraph.sep
        ]
    else if Conf.is_markdown config then
      choice
        [ Table.parse config
        ; Type_parser.Block.parse config
        ; Hr.parse config
        ; Paragraph.parse
        ; Paragraph.sep
        ]
    else
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

let org_full_parsers config =
  [ Paragraph.sep
  ; Directive.parse
  ; Drawer.parse config
  ; Type_parser.Heading.parse config
  ; Table.parse config
  ; Latex_env.parse config
  ; Type_parser.Block.parse config
  ; Footnote.parse config
  ; Type_parser.Lists.parse config (list_content_parsers config)
  ; Hr.parse config
  ; Type_parser.Block.results
  ; Comment.parse config
  ; Paragraph.parse
  ]

let md_full_parsers config =
  [ Paragraph.sep
  ; Type_parser.Heading.parse config
  ; Drawer.parse config
  ; Table.parse config
  ; Latex_env.parse config
  ; Type_parser.Block.parse config
  ; Footnote.parse config
  ; Type_parser.Lists.parse config (list_content_parsers config)
  ; Hr.parse config
  ; Paragraph.parse
  ]

let org_outline_parsers config =
  [ Paragraph.sep
  ; Directive.parse
  ; Drawer.parse config
  ; Type_parser.Heading.parse config
  ; Type_parser.Block.parse config
  ; Footnote.parse config
  ; Type_parser.Lists.parse config (list_content_parsers config)
  ; Type_parser.Block.results
  ; Paragraph.parse
  ]

let line_has_colon_colon s =
  let n = String.length s in
  let rec loop i =
    if i + 1 >= n then
      false
    else if s.[i] = ':' && s.[i + 1] = ':' then
      true
    else
      loop (i + 1)
  in
  loop 0

(** Markdown outline: peek-dispatch to avoid choice backtracking on every line. *)
let md_outline_block config =
  let heading = Type_parser.Heading.parse config in
  let lists = Type_parser.Lists.parse config (list_content_parsers config) in
  let drawer = Drawer.parse config in
  let block = Type_parser.Block.parse config in
  let footnote = Footnote.parse config in
  peek_char >>= function
  | None -> fail "eof"
  | Some '\n'
  | Some '\r' ->
    Paragraph.sep
  | Some '-' -> heading <|> lists <|> Paragraph.parse
  | Some '#' -> heading <|> Paragraph.parse
  | Some '+'
  | Some '*' ->
    lists <|> Paragraph.parse
  | Some ' '
  | Some '\t' ->
    drawer <|> block <|> lists <|> footnote <|> Paragraph.parse
  | Some '`'
  | Some '>' ->
    block <|> Paragraph.parse
  | Some '[' -> footnote <|> Paragraph.parse
  | Some ':' -> drawer <|> Paragraph.parse
  | _ ->
    (* Plain or property line (key::). Skip Drawer when no `::`. *)
    peek_line >>= fun line ->
    if line_has_colon_colon line then
      drawer <|> Paragraph.parse
    else
      Paragraph.parse

let md_front_matter_parse parse =
  Markdown_front_matter.parse >>= fun fm_result ->
  parse >>= fun result -> return (List.append fm_result result)

let build_choice_parsers parsers config =
  let parsers = parsers config in
  let choice = choice parsers in
  let p =
    if config.Conf.parse_outline_only then
      choice >>| fun t -> (t, Pos.dummy_pos)
    else
      Helper.with_pos_meta choice
  in
  let parse = many p in
  if config.Conf.parse_outline_only && Conf.is_markdown config then
    parse
  else
    md_front_matter_parse parse <|> parse

let build_md_outline_parsers config =
  let p = md_outline_block config >>| fun t -> (t, Pos.dummy_pos) in
  many p

let parse config input =
  let outline_only = Conf.(config.parse_outline_only) in
  let md = Conf.is_markdown config in
  (* Outline markdown uses the line scanner. Full markdown stays on Angstrom so
     mixed constructs (org blocks, drawers, definition lists, quotes) match
     published mldoc / Logseq graph-parser. *)
  if md && outline_only then
    let ast = Md_outline.parse config input in
    if String.contains input '\\' then
      List.map (fun (t, pos) -> (Type_op.md_unescaped t, pos)) ast
    else
      ast
  else
    let parsers =
      if md then
        build_choice_parsers md_full_parsers config
      else if outline_only then
        build_choice_parsers org_outline_parsers config
      else
        build_choice_parsers org_full_parsers config
    in
    match parse_string ~consume:All parsers input with
    | Ok result ->
      let ast = Paragraph.concat_paragraph_lines config result in
      let ast =
        if md then
          List.map (fun (t, pos) -> (Type_op.md_unescaped t, pos)) ast
        else
          ast
      in
      let ast =
        if (not md) && outline_only then
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
      ast
    | Error err -> failwith err

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

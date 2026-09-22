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

type md_prop_line =
  | Mp_property of string * string (* key, raw value *)
  | Mp_hashplus of string * string (* #+NAME: value *)
  | Mp_colon (* starts with ':' — general drawer territory *)
  | Mp_other

(* Classify a Markdown property line: `key:: value`, `key::`, or `#+NAME: v`.
   Mirrors Markdown_property.property + Drawer.parse2 (property tried first). *)
let classify_md_prop_line line i n =
  let stop = i + n in
  let rec skip_sp j =
    if j < stop && is_space line.[j] then
      skip_sp (j + 1)
    else
      j
  in
  (* property key: take_while1 (c <> ':' && non_space_eol) *)
  let find_key_end i =
    let rec scan k =
      if k >= stop then
        None
      else if line.[k] = ':' then
        Some k
      else if is_space line.[k] || is_eol line.[k] then
        None
      else
        scan (k + 1)
    in
    scan i
  in
  let i0 = skip_sp i in
  if i0 >= stop then
    Mp_other
  else
    match find_key_end i0 with
    | Some k when k > i0 && k + 1 < stop && line.[k + 1] = ':' ->
      (* `key::` found *)
      let v = k + 2 in
      if v < stop && line.[v] = ' ' then
        let v' = skip_sp (v + 1) in
        Mp_property (String.sub line i0 (k - i0), String.sub line v' (stop - v'))
      else
        let v' = skip_sp v in
        if v' >= stop then
          Mp_property (String.sub line i0 (k - i0), "")
        else
          Mp_other
    | _ ->
      if i0 + 1 < stop && line.[i0] = '#' && line.[i0 + 1] = '+' then
        (* #+NAME: value *)
        let j = i0 + 2 in
        match find_key_end j with
        | Some k when k > j ->
          let v = skip_sp (k + 1) in
          Mp_hashplus (String.sub line j (k - j), String.sub line v (stop - v))
        | _ -> Mp_other
      else if line.[i0] = ':' then
        Mp_colon
      else
        Mp_other

(** Full Markdown: same idea as [md_outline_block] — dispatch on the first
    non-blank char of the line so most lines run a single parser instead of
    backtracking through the whole choice. Candidate order within each branch
    preserves [md_full_parsers] ordering. *)
let md_full_block config =
  let heading = Type_parser.Heading.parse config in
  let drawer = Drawer.parse config in
  let table = Table.parse config in
  let latex_env = Latex_env.parse config in
  let block = Type_parser.Block.parse config in
  let footnote = Footnote.parse config in
  let lists = Type_parser.Lists.parse config (list_content_parsers config) in
  let hr = Hr.parse config in
  peek_char >>= function
  | None -> fail "eof"
  | Some '\n'
  | Some '\r' ->
    Paragraph.sep
  | _ -> (
    peek_line >>= fun line ->
    let n = String.length line in
    let rec skip_blank i =
      if i < n && (line.[i] = ' ' || line.[i] = '\t') then
        skip_blank (i + 1)
      else
        i
    in
    let i = skip_blank 0 in
    if i >= n then
      (* whitespace-only line *)
      Paragraph.parse
    else
      let consume_line t =
        advance n *> optional (end_of_line <|> end_of_input) *> return t
      in
      match line.[i] with
      | '#' -> (
        match Type_parser.Heading.try_parse_md_line config line with
        | Some t -> consume_line t
        | None -> heading <|> drawer <|> block <|> Paragraph.parse)
      | '-' -> (
        match Type_parser.Heading.try_parse_md_line config line with
        | Some t -> consume_line t
        | None -> heading <|> drawer <|> hr <|> Paragraph.parse)
      | '*' -> drawer <|> lists <|> hr <|> Paragraph.parse
      | '+' -> drawer <|> lists <|> Paragraph.parse
      | '`'
      | '~'
      | '>'
      | '<'
      | '$' ->
        drawer <|> block <|> Paragraph.parse
      | '\\' -> drawer <|> latex_env <|> block <|> Paragraph.parse
      | '|' -> drawer <|> table <|> Paragraph.parse
      | '[' -> drawer <|> block <|> footnote <|> Paragraph.parse
      | ':' -> drawer <|> Paragraph.parse
      | '_' -> drawer <|> hr <|> Paragraph.parse
      | '0' .. '9' -> drawer <|> lists <|> Paragraph.parse
      | _ ->
        (* A plain line can open a definition list when the next line is a
           `: definition` item; try Lists first in that case. *)
        let next_is_def =
          available >>= fun avail ->
          let len = min avail (n + 4) in
          peek_string len >>| fun s ->
          let rec next_char_is_colon i =
            if i >= len then
              false
            else
              match s.[i] with
              | ' '
              | '\t' ->
                next_char_is_colon (i + 1)
              | c -> c = ':'
          in
          if n >= len || (s.[n] <> '\n' && s.[n] <> '\r') then
            false
          else if s.[n] = '\r' && n + 1 < len && s.[n + 1] = '\n' then
            next_char_is_colon (n + 2)
          else
            next_char_is_colon (n + 1)
        in
        next_is_def >>= fun def ->
        if def then
          drawer <|> lists <|> Paragraph.parse
        else
          drawer <|> Paragraph.parse)

(* Full Markdown: drive the parse with a plain line scan — headings,
   properties, blank runs and paragraph lines are handled in pure OCaml;
   anything more complex falls back to [md_full_block] (the same per-char
   dispatch, on Angstrom) run at the current offset. Positions stay absolute
   because the fallback parses the whole input after [advance cur]. *)
let md_full_parse_raw config input =
  let n = String.length input in
  let bs = lazy (Bigstringaf.of_string ~off:0 ~len:n input) in
  let run_at p cur =
    match Unbuffered.parse (advance cur *> p) with
    | Unbuffered.Partial { Unbuffered.continue; _ } -> (
      match continue (Lazy.force bs) ~off:0 ~len:n Unbuffered.Complete with
      | Unbuffered.Done (end_pos, t) -> `Ok (t, end_pos)
      | _ -> `Fail)
    | _ -> `Fail
  in
  let block_p = Helper.with_pos_meta (md_full_block config) in
  let line_end i =
    let rec scan j =
      if j >= n || input.[j] = '\r' || input.[j] = '\n' then
        j
      else
        scan (j + 1)
    in
    scan i
  in
  (* position after one eol sequence starting at i (i must sit on \r/\n,
     or be at eof) *)
  let after_eol i =
    if i >= n then
      i
    else if input.[i] = '\r' && i + 1 < n && input.[i + 1] = '\n' then
      i + 2
    else
      i + 1
  in
  let skip_blanks i =
    let rec scan j =
      if j < n && (input.[j] = '\r' || input.[j] = '\n') then
        scan (after_eol j)
      else
        j
    in
    scan i
  in
  let first_non_blank i le =
    let rec scan j =
      if j < le && (input.[j] = ' ' || input.[j] = '\t') then
        scan (j + 1)
      else
        j
    in
    scan i
  in
  let line i le = String.sub input i (le - i) in
  (* Maximal run of `key::` / `#+NAME:` lines starting at i (whose line is
     known to be one). `Fail` defers to the Angstrom fallback: a ':'-starting
     line mid-run (drawer_parse / :PROPERTIES: merge cases). *)
  let property_run i =
    let rec go cur acc =
      if cur >= n then
        `Ok (List.rev acc, cur)
      else
        let le = line_end cur in
        match classify_md_prop_line input cur (le - cur) with
        | Mp_property (k, v_raw) ->
          let v = String.trim v_raw in
          let refs = Property.property_references config v in
          go (after_eol le) ((k, v, refs) :: acc)
        | Mp_hashplus (k, v) ->
          (* the trailing eols of between_eols absorb following blank lines *)
          go (skip_blanks (after_eol le)) ((k, v, []) :: acc)
        | Mp_colon -> `Fail
        | Mp_other ->
          if le = cur then
            (* blank line: the run continues only when a #+ line follows *)
            let p = skip_blanks cur in
            let le' = line_end p in
            match classify_md_prop_line input p (le' - p) with
            | Mp_hashplus (k, v) ->
              go (skip_blanks (after_eol le')) ((k, v, []) :: acc)
            | _ -> `Ok (List.rev acc, cur)
          else
            `Ok (List.rev acc, cur)
    in
    go i []
  in
  let mk_pos t s e = (t, { Pos.start_pos = s; end_pos = e }) in
  let rec loop acc cur =
    if cur >= n then
      List.rev acc
    else if input.[cur] = '\r' || input.[cur] = '\n' then
      let e = skip_blanks cur in
      loop (mk_pos (Type.Paragraph_Sep (e - cur)) cur e :: acc) e
    else
      let le = line_end cur in
      let i = first_non_blank cur le in
      let line_str = lazy (line cur le) in
      if i >= le then
        (* whitespace-only line *)
        loop
          (mk_pos (Type.Paragraph_line (Lazy.force line_str)) cur le :: acc)
          le
      else
        match input.[i] with
        | '#'
        | '-' -> (
          match
            Type_parser.Heading.try_parse_md_line config (Lazy.force line_str)
          with
          | Some t ->
            let e =
              if le < n then
                after_eol le
              else
                le
            in
            loop (mk_pos t cur e :: acc) e
          | None -> fallback acc cur)
        | _ -> (
          match classify_md_prop_line input cur (le - cur) with
          | Mp_property _
          | Mp_hashplus _ -> (
            match property_run cur with
            | `Ok (kvs, e) ->
              loop (mk_pos (Type.Property_Drawer kvs) cur e :: acc) e
            | `Fail -> fallback acc cur)
          | _ -> (
            match input.[i] with
            | '*'
            | '+'
            | '`'
            | '~'
            | '>'
            | '<'
            | '$'
            | '\\'
            | '|'
            | '['
            | ':'
            | '_'
            | '0' .. '9' ->
              fallback acc cur
            | _ ->
              (* next line's first non-blank char ':' opens a definition
                 list, which outranks a plain paragraph *)
              let p =
                if le < n then
                  after_eol le
                else
                  n
              in
              let j =
                if p < n then
                  first_non_blank p (line_end p)
                else
                  n
              in
              if j < n && input.[j] = ':' then
                fallback acc cur
              else
                loop
                  (mk_pos (Type.Paragraph_line (Lazy.force line_str)) cur le
                  :: acc)
                  le))
  and fallback acc cur =
    match run_at block_p cur with
    | `Ok ((t, pos), e) -> loop ((t, pos) :: acc) e
    | `Fail ->
      (* unreachable: every branch ends in Paragraph.parse, which cannot
         fail on a non-empty line *)
      let le = line_end cur in
      loop (mk_pos (Type.Paragraph_line (line cur le)) cur le :: acc) le
  in
  match run_at Markdown_front_matter.parse 0 with
  | `Ok (fm, cur) -> fm @ loop [] cur
  | `Fail -> loop [] 0

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
  (* Markdown full parse runs the line-scan driver; outline-only markdown the
     dedicated scanner; org keeps the Angstrom choices. *)
  if md && outline_only then
    let ast = Md_outline.parse config input in
    if String.contains input '\\' then
      List.map (fun (t, pos) -> (Type_op.md_unescaped t, pos)) ast
    else
      ast
  else if md then
    (* line-scan driver with per-block Angstrom fallback *)
    let result = md_full_parse_raw config input in
    let ast = Paragraph.concat_paragraph_lines config result in
    List.map (fun (t, pos) -> (Type_op.md_unescaped t, pos)) ast
  else
    let parsers =
      if outline_only then
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

open Angstrom
open Parsers
open Prelude
open Type
open Conf

module MakeHeading (Block : sig
  val parse : Conf.t -> Type.t Angstrom.t
end) =
struct
  (* TODO: Markdown alternate syntax,
     https://www.markdownguide.org/basic-syntax/#alternate-syntax
  *)

  (* todo keywords *)
  let marker =
    string "TODO" <|> string "DOING" <|> string "WAITING" <|> string "WAIT"
    <|> string "DONE" <|> string "CANCELED" <|> string "CANCELLED"
    <|> string "STARTED" <|> string "IN-PROGRESS" <|> string "NOW"
    <|> string "LATER"
    >>= fun s ->
    peek_char >>= function
    | None -> return s
    | Some c ->
      if c == ' ' then
        return s
      else
        fail "Marker should followed by some spaces"

  let org_level = take_while1 (fun c -> c = '*')

  (* return (level, is_unordered, size) *)
  let level config =
    let result =
      match config.format with
      | Org ->
        org_level >>= fun s ->
        let len = String.length s in
        return (len, true, None)
      | Markdown ->
        let markdown_heading =
          Markdown_level.parse >>| fun (indents, s) ->
          let len = String.length s in
          ( Option.map_default
              (fun indents -> String.length indents + 1)
              1 indents
          , false
          , Some len )
        in
        let unordered =
          lift2
            (fun result size ->
              match (result, size) with
              | Some s, None ->
                let len = String.length s in
                (len + 1, true, None)
              | None, None -> (1, true, None)
              | Some s, Some size ->
                let len = String.length s in
                (len + 1, true, Some (String.length size))
              | None, Some size -> (1, true, Some (String.length size)))
            (optional tabs_or_ws <* char '-')
            (optional
            @@ (spaces *> take_while1 (fun c -> c = '#')
               <* (unsafe_lookahead (satisfy is_space_eol) *> return ()
                  <|> end_of_input)))
        in
        markdown_heading <|> unordered
    in
    result
    <* ( peek_char >>= fun c ->
         match c with
         | None -> return ()
         | Some c' when List.mem c' whitespace_chars -> return ()
         | _ -> fail "need whitespace after (#|*|-)" )

  let priority = string "[#" *> any_char <* char ']'

  let seperated_tags =
    sep_by (char ':') (take_while1 (fun x -> x <> ':' && non_space_eol x))

  let tags = char ':' *> seperated_tags <* char ':'

  let title_aux_p config =
    let config = { config with Conf.hiccup_in_block = false } in
    if config.parse_outline_only then
      (* Only run Block when the title might be a fence/quote. *)
      Angstrom.unsafe_lookahead
        (peek_char >>= function
         | Some '`'
         | Some '>' ->
           Block.parse config <|> Paragraph.parse
         | _ -> Paragraph.parse)
    else if Conf.is_markdown config then
      (* Markdown: most titles are plain lines; dispatch on the first
         non-blank char instead of backtracking through every parser. *)
      Angstrom.unsafe_lookahead
        ( peek_line >>= fun line ->
          let n = String.length line in
          let rec skip_blank i =
            if i < n && (line.[i] = ' ' || line.[i] = '\t') then
              skip_blank (i + 1)
            else
              i
          in
          let i = skip_blank 0 in
          let plain = Paragraph.parse in
          if i >= n then
            plain
          else
            match line.[i] with
            | ':'
            | '#' ->
              Drawer.parse config <|> Block.parse config <|> plain
            | '`'
            | '~'
            | '>'
            | '<'
            | '$'
            | '\\' ->
              Block.parse config <|> plain
            | '[' -> Block.parse config <|> Footnote.parse config <|> plain
            | _ ->
              let rec has_colon_colon j =
                if j + 1 >= n then
                  false
                else if line.[j] = ':' && line.[j + 1] = ':' then
                  true
                else
                  has_colon_colon (j + 1)
              in
              if has_colon_colon i then
                Drawer.parse config <|> plain
              else
                plain )
    else
      Angstrom.unsafe_lookahead
        (choice
           [ Drawer.parse config
           ; Hr.parse config
           ; Table.parse config
           ; Latex_env.parse config
           ; Block.parse config
           ; Footnote.parse config
           ; Paragraph.parse
           ])

  (* not include priority, tags, marker
     return (title_line_string, first Type.t) *)
  (* Peeked-line classification for a Markdown title line. Returns the parser
     to run as a lookahead guard, or `None` when the line is plain text. *)
  let md_title_guard config line i n =
    if i >= n then
      None
    else
      match line.[i] with
      | ':'
      | '#' ->
        Some (Drawer.parse config <|> Block.parse config)
      | '`'
      | '~'
      | '>'
      | '<'
      | '$'
      | '\\' ->
        Some (Block.parse config)
      | '[' -> Some (Block.parse config <|> Footnote.parse config)
      | _ ->
        let rec has_colon_colon j =
          if j + 1 >= n then
            false
          else if line.[j] = ':' && line.[j + 1] = ':' then
            true
          else
            has_colon_colon (j + 1)
        in
        if has_colon_colon i then
          Some (Drawer.parse config)
        else
          None

  let title config =
    if Conf.is_markdown config && not config.parse_outline_only then
      (* Peek the title line once; plain titles are returned directly instead
         of being scanned twice (lookahead + `line`). *)
      peek_line >>= fun l ->
      let n = String.length l in
      let rec skip_blank i =
        if i < n && (l.[i] = ' ' || l.[i] = '\t') then
          skip_blank (i + 1)
        else
          i
      in
      match md_title_guard config l (skip_blank 0) n with
      | None -> advance n *> return l
      | Some guard -> (
        unsafe_lookahead (guard <|> Paragraph.parse) >>= fun t ->
        match t with
        | Paragraph_line _ -> advance n *> return l
        | _ -> return "")
    else
      title_aux_p config >>= fun t ->
      match t with
      | Paragraph_line _ -> line
      | _ -> return ""

  let is_blank s =
    let n = String.length s in
    let rec aut_is_blank i =
      if i = n then
        true
      else
        let c = s.[i] in
        if is_space c then
          aut_is_blank (i + 1)
        else
          false
    in
    aut_is_blank 0

  let anchor_link s =
    let s = String.trim s in
    let b = Buffer.create (String.length s + 8) in
    String.iter
      (fun c ->
        match c with
        | 'a' .. 'z'
        | 'A' .. 'Z'
        | '0' .. '9'
        | '('
        | ')' ->
          Buffer.add_char b c
        | ' '
        | '_'
        | '-' ->
          Buffer.add_char b '_'
        | c -> Buffer.add_string b (Printf.sprintf "-%x-" (int_of_char c)))
      s;
    Buffer.contents b

  let outline_title config title =
    if title = "" then
      []
    else if Outline_inline.may_have_outline_markup config title then
      match parse_string ~consume:All (Outline_inline.parse config) title with
      | Ok title -> title
      | Error _ -> Type_op.inline_list_with_none_pos [ Inline.Plain title ]
    else
      Type_op.inline_list_with_none_pos [ Inline.Plain title ]

  let make_outline_heading ~level ~unordered ~size ~marker ~priority ~title =
    Heading
      { level
      ; marker
      ; priority
      ; title
      ; tags = []
      ; anchor = ""
      ; meta = { timestamps = []; properties = [] }
      ; numbering = None
      ; unordered
      ; size
      }

  (** Fast MD outline heading: reuse [level], skip title_aux Block/Drawer. *)
  let parse_md_outline config =
    level config <?> "Heading level" >>= fun (level, unordered, size) ->
    (if not config.parse_marker then
       return None
     else
       optional (spaces *> marker <?> "Heading marker"))
    >>= fun marker ->
    (if not config.parse_priority then
       return None
     else
       optional (spaces *> priority <?> "Heading priority"))
    >>= fun priority ->
    optional spaces *> peek_char >>= function
    | Some '`'
    | Some '>' ->
      (* Leave fence/quote on the line for Block.parse. *)
      return
        (make_outline_heading ~level ~unordered ~size ~marker ~priority
           ~title:[])
    | None ->
      return
        (make_outline_heading ~level ~unordered ~size ~marker ~priority
           ~title:[])
    | Some c when is_eol c ->
      return
        (make_outline_heading ~level ~unordered ~size ~marker ~priority
           ~title:[])
      <* optional eol
    | _ ->
      optional_line >>= fun title ->
      return
        (make_outline_heading ~level ~unordered ~size ~marker ~priority
           ~title:(outline_title config title))
      <* optional (end_of_line <|> end_of_input)

  let md_markers =
    [ "TODO"
    ; "DOING"
    ; "WAITING"
    ; "WAIT"
    ; "DONE"
    ; "CANCELED"
    ; "CANCELLED"
    ; "STARTED"
    ; "IN-PROGRESS"
    ; "NOW"
    ; "LATER"
    ]

  (** Pure-OCaml Markdown heading on a single (peeked) line — mirrors the
      Angstrom [parse] path for [format = Markdown]: level (ATX "#"s or "-"),
      optional marker, optional priority, and a plain-text title.
      Returns [None] when the line is not a heading or when the title may
      start a nested construct (drawer / block / footnote) that the caller
      must let the general parsers handle. *)
  let try_parse_md_line config line =
    if (not (Conf.is_markdown config)) || config.parse_outline_only then
      None
    else
      let n = String.length line in
      (* is_space: ' ' '\t' '\026' '\012'; whitespace check after the level
         uses [whitespace_chars]: ' ' '\t' '\n' '\r' '\012' *)
      let is_sp c = is_space c in
      let is_ws_after c =
        c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = '\012'
      in
      let rec skip_sp i =
        if i < n && is_sp line.[i] then
          skip_sp (i + 1)
        else
          i
      in
      let i0 = skip_sp 0 in
      if i0 >= n then
        None
      else
        let indents = i0 in
        let level_unord_size =
          if line.[i0] = '#' then
            let rec count k =
              if k < n && line.[k] = '#' then
                count (k + 1)
              else
                k
            in
            let e = count i0 in
            Some (indents + 1, false, Some (e - i0), e)
          else if line.[i0] = '-' then
            let k = i0 + 1 in
            let k' = skip_sp k in
            if k' < n && line.[k'] = '#' then
              let rec count k =
                if k < n && line.[k] = '#' then
                  count (k + 1)
                else
                  k
              in
              let e = count k' in
              if e >= n || is_space_eol line.[e] then
                Some (indents + 1, true, Some (e - k'), e)
              else
                Some (indents + 1, true, None, k)
            else
              Some (indents + 1, true, None, k)
          else
            None
        in
        match level_unord_size with
        | None -> None
        | Some (level, unordered, size, j) ->
          if
            (* require whitespace or EOI after the level *)
            j < n && not (is_ws_after line.[j])
          then
            None
          else
            (* Each of marker / priority / title is an optional
               `ws *> p`: whitespace is required but backtracks when the
               inner parser fails, so each retry starts before the ws. *)
            let after_level = j in
            let j1 = skip_sp after_level in
            let marker, j2 =
              if config.parse_marker && j1 > after_level then
                let rec try_markers = function
                  | [] -> (None, after_level)
                  | m :: ms ->
                    let ml = String.length m in
                    if
                      j1 + ml <= n
                      && String.sub line j1 ml = m
                      && (j1 + ml = n || line.[j1 + ml] = ' ')
                    then
                      (Some m, j1 + ml)
                    else
                      try_markers ms
                in
                try_markers md_markers
              else
                (None, after_level)
            in
            let j3 = skip_sp j2 in
            let priority, j4 =
              if
                config.parse_priority && j3 > j2
                && j3 + 3 < n
                && line.[j3] = '['
                && line.[j3 + 1] = '#'
                && line.[j3 + 3] = ']'
              then
                (Some line.[j3 + 2], j3 + 4)
              else
                (None, j2)
            in
            let j = skip_sp j4 in
            let title_str =
              if j > j4 && j < n then
                String.sub line j (n - j)
              else
                ""
            in
            (* Defer to the general parsers when the title could start a
               nested drawer/block/footnote (same guard as [title]). *)
            let title_needs_general =
              if n - j <= 0 then
                false
              else
                match line.[j] with
                | ':'
                | '#'
                | '`'
                | '~'
                | '>'
                | '<'
                | '$'
                | '\\' ->
                  true
                | '[' -> n - j >= 2 && (line.[j + 1] = '^' || line.[j + 1] = ':')
                | _ ->
                  let rec has_colon_colon k =
                    if k + 1 >= n then
                      false
                    else if line.[k] = ':' && line.[k + 1] = ':' then
                      true
                    else
                      has_colon_colon (k + 1)
                  in
                  has_colon_colon j
            in
            if title_needs_general then
              None
            else
              let title =
                if title_str = "" then
                  []
                else
                  match Inline.parse_opt config title_str with
                  | Some title -> title
                  | None -> []
              in
              let anchor =
                anchor_link
                  (Inline.asciis (Type_op.inline_list_strip_pos title))
              in
              Some
                (Heading
                   { level
                   ; marker
                   ; priority
                   ; title
                   ; tags = []
                   ; anchor
                   ; meta = { timestamps = []; properties = [] }
                   ; numbering = None
                   ; unordered
                   ; size
                   })

  let parse config =
    if config.parse_outline_only && Conf.is_markdown config then
      parse_md_outline config
    else
      let p =
        lift4
          (fun (level, unordered, size) marker priority pos_and_title ->
            let title =
              match pos_and_title with
              | None -> []
              | Some (_pos, title) -> (
                if config.parse_outline_only then
                  outline_title config title
                else
                  match Inline.parse_opt config title with
                  | Some title -> title
                  | None -> [])
            in
            let title, tags =
              match title with
              | [] -> (title, [])
              | _ -> (
                match config.format with
                | Org -> (
                  let last_inline = List.nth title (List.length title - 1) in
                  match last_inline with
                  | Inline.Plain s, _ ->
                    let s = String.trim s in
                    if String.length s > 1 && s.[String.length s - 1] = ':' then
                      let prefix, maybe_tags = splitr (fun c -> c <> ' ') s in
                      match parse_string ~consume:All tags maybe_tags with
                      | Ok tags ->
                        let title =
                          if prefix = "" then
                            drop_last 1 title
                          else
                            drop_last 1 title
                            @ Type_op.inline_list_with_none_pos
                                [ Inline.Plain prefix ]
                        in
                        let open Option in
                        let last_plain =
                          List.nth_opt title (List.length title - 1)
                          >>| fun (inline_t, pos) ->
                          ( (match inline_t with
                            | Inline.Plain s ->
                              Inline.Plain (String.rtrim s ^ " ")
                            | _ -> inline_t)
                          , pos )
                        in
                        let title' =
                          if Option.is_some last_plain then
                            let _, butlast_title = butlast title in
                            List.append butlast_title [ Option.get last_plain ]
                          else
                            title
                        in
                        (title', remove is_blank tags)
                      | _ -> (title, [])
                    else
                      (title, [])
                  | _ -> (title, []))
                | Markdown -> (title, []))
            in
            let anchor =
              if config.parse_outline_only then
                ""
              else
                anchor_link
                  (Inline.asciis (Type_op.inline_list_strip_pos title))
            in
            let meta = { timestamps = []; properties = [] } in
            Heading
              { level
              ; marker
              ; priority
              ; title
              ; tags
              ; anchor
              ; meta
              ; numbering = None
              ; unordered
              ; size
              })
          (level config <?> "Heading level")
          (if not config.parse_marker then
             return None
           else
             optional (ws *> marker <?> "Heading marker"))
          (if not config.parse_priority then
             return None
           else
             optional (ws *> priority <?> "Heading priority"))
          (optional
             (ws *> Angstrom.both pos (title config) <?> "Heading title"))
      in
      p <* optional (end_of_line <|> end_of_input)
end

open Angstrom
open Parsers
open Prelude
open Type
open Conf

module MakeHeading (Lists : sig
  val parse :
    Conf.t -> (Type.t * Pos.pos_meta) list Angstrom.t -> Type.t Angstrom.t
end) (Block : sig
  val parse : Conf.t -> Type.t Angstrom.t

  val results : Type.t Angstrom.t
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
          Markdown_level.parse >>| fun s ->
          let len = String.length s in
          (len, false, Some len)
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
    Angstrom.unsafe_lookahead
      (choice
         [ Drawer.parse config
         ; Paragraph.sep
         ; Table.parse config
         ; Latex_env.parse config
         ; Lists.parse config
             (choice
                [ Table.parse config
                ; Block.parse config
                ; Latex_env.parse config
                ; Hr.parse config
                ; Block.results
                ; Comment.parse config
                ; Paragraph.parse
                ; Paragraph.sep
                ]
             |> Helper.with_pos_meta |> many1)
         ; Block.parse config
         ; Hr.parse config
         ; Block.results
         ; Footnote.parse config
         ; Comment.parse config
         ; Paragraph.parse
         ])

  (* not include priority, tags, marker
     return (title_line_string, first Type.t) *)
  let title config =
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
    let map_char = function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '(' | ')') as c ->
        String.make 1 c
      | ' '
      | '_'
      | '-' ->
        "_"
      | c -> Printf.sprintf "-%x-" (int_of_char c)
    in
    explode (String.trim s) |> List.map map_char |> String.concat ""

  let parse config =
    let p =
      lift4
        (fun (level, unordered, size) marker priority pos_and_title ->
          let title =
            match pos_and_title with
            | None -> []
            | Some (_pos, title) -> (
              match parse_string ~consume:All (Inline.parse config) title with
              | Ok title -> title
              | Error _e -> [])
          in
          let title, tags =
            match title with
            | [] -> (title, [])
            | _ -> (
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
                        @ Type_op.inline_list_with_dummy_pos
                            [ Inline.Plain prefix ]
                    in
                    (title, remove is_blank tags)
                  | _ -> (title, [])
                else
                  (title, [])
              | _ -> (title, []))
          in
          let anchor =
            anchor_link (Inline.asciis (Type_op.inline_list_strip_pos title))
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
        (optional (ws *> marker <?> "Heading marker"))
        (optional (ws *> priority <?> "Heading priority"))
        (optional (ws *> Angstrom.both pos (title config) <?> "Heading title"))
    in
    p <* optional (end_of_line <|> end_of_input)
end

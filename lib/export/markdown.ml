open Prelude
open Type
open Inline
open Document
open Reference
open Conf

type t =
  | RawText of string
  (* merge adjacent multiple Space as one Space *)
  | Space
  (* merge adjacent multiple Newline as one Newline *)
  | Newline
  | TwoNewlines
  | OneNewline (* merge_adjacent_space_newline internal use *)
  | Indent of int

let show t =
  match t with
  | RawText s -> "RawText:\"" ^ s ^ "\""
  | Space -> "Space"
  | Newline -> "Newline"
  | TwoNewlines -> "TwoNewlines"
  | OneNewline -> "OneNewline"
  | Indent n -> Printf.sprintf "Indent(%d)" n

let raw_text s = RawText s

let newline = Newline

let map_raw_text = List.map raw_text

let flatten_map f l = List.flatten (CCList.map f l)

type refs = Reference.parsed_t

type state =
  { outside_em_symbol : char option
  ; embed_history : string list
  ; embed_parent_indent_level : int
  ; mutable current_level : int
  ; mutable top_heading_level : int option
  }

let default_state () =
  { outside_em_symbol = None
  ; embed_parent_indent_level = 0
  ; current_level = 0
  ; embed_history = []
  ; top_heading_level = None
  }

let default_config = None

let raw_text_indent state config s =
  let indent state config s =
    if config.heading_to_list then
      let ls =
        lines s
        |> flatten_map (fun l ->
               [ Indent (state.current_level + 1); raw_text (l ^ "\n") ])
      in
      let ls_rev = List.rev ls in
      List.rev
        (match ls_rev with
        | RawText s :: tl ->
          newline :: raw_text (String.sub s 0 (String.length s - 1)) :: tl
        | _ -> ls_rev)
    else
      [ raw_text s ]
  in
  if state.current_level = 0 then
    [ raw_text s ]
  else
    Indent (state.current_level + 1) :: indent state config s

let rec inline refs state config (t : Inline.t) : t list =
  Indent (state.current_level + 1)
  ::
  (match t with
  | Emphasis em -> emphasis refs state config em
  | Break_Line -> [ raw_text "\n" ]
  | Hard_Break_Line -> [ raw_text "  \n" ]
  | Verbatim s -> [ raw_text s ]
  | Code s -> map_raw_text [ "`"; s; "`" ] (* it's inline code *)
  | Tag s -> map_raw_text [ "#"; s ]
  | Spaces s -> map_raw_text [ s ]
  | Plain s -> map_raw_text [ s ]
  | Link l -> inline_link l
  | Nested_link l -> inline_nested_link l
  | Target s -> map_raw_text [ "<<"; s; ">>" ]
  | Subscript tl -> inline_subscript refs state config tl
  | Superscript tl -> inline_superscript refs state config tl
  | Footnote_Reference fr -> footnote_reference fr
  | Cookie c -> cookie c
  | Latex_Fragment lf -> latex_fragment lf
  | Macro m -> macro refs state config m
  | Entity e -> entity e
  | Timestamp t -> timestamp t
  | Radio_Target s ->
    map_raw_text [ "<<<"; s; ">>>" ] (* FIXME: not parsed in md parser? *)
  | Export_Snippet (name, content) ->
    [ raw_text "@@"
    ; raw_text name
    ; raw_text ":"
    ; Space
    ; raw_text content
    ; raw_text "@@"
    ]
  | Inline_Source_Block { language; options; code } ->
    map_raw_text [ "src_"; language; "["; options; "]{"; code; "}" ]
  | Email e -> map_raw_text [ "<"; Email_address.to_string e; ">" ]
  | Block_reference uuid -> block_reference refs state config uuid
  | Inline_Hiccup s -> map_raw_text [ s ]
  | Inline_Html s -> map_raw_text [ s ])

and emphasis refs state config (typ, tl) =
  let outside_em_symbol = state.outside_em_symbol in
  let wrap_with tl s =
    let outside_em_symbol =
      match s.[0] with
      | ('*' | '_') as e -> Some e
      | _ -> None
    in
    List.flatten
      [ [ raw_text s ]
      ; List.flatten
        @@ CCList.map (inline refs { state with outside_em_symbol } config) tl
      ; [ raw_text s ]
      ]
  in
  match typ with
  | `Bold ->
    wrap_with tl
      (if outside_em_symbol = Some '*' then
        "__"
      else
        "**")
  | `Strike_through -> wrap_with tl "~~"
  | `Highlight -> wrap_with tl "^^"
  | `Italic ->
    wrap_with tl
      (if outside_em_symbol = Some '*' then
        "_"
      else
        "*")
  | `Underline ->
    List.flatten
    @@ CCList.map
         (fun e ->
           Space :: inline refs { state with outside_em_symbol } config e)
         tl

and inline_link { full_text; _ } = [ raw_text full_text ]

and inline_nested_link { content; _ } = [ raw_text content ]

and inline_subscript refs state config tl =
  List.flatten
    [ [ raw_text "_{" ]
    ; flatten_map (fun e -> Space :: inline refs state config e) tl
    ; [ raw_text "}" ]
    ]

and inline_superscript refs state config tl =
  List.flatten
    [ [ raw_text "^{" ]
    ; flatten_map (fun e -> Space :: inline refs state config e) tl
    ; [ raw_text "}" ]
    ]

and footnote_reference { name; _ } = map_raw_text [ "["; name; "]" ]

and cookie = function
  | Percent v -> map_raw_text [ "["; string_of_int v; "%]" ]
  | Absolute (current, total) ->
    map_raw_text [ "["; string_of_int current; "/"; string_of_int total; "]" ]

and latex_fragment = function
  | Inline s -> [ Space; raw_text "$"; raw_text s; raw_text "$"; Space ]
  | Displayed s -> [ Space; raw_text "$$"; raw_text s; raw_text "$$"; Space ]

and macro refs state config m =
  if m.name <> "embed" then
    map_raw_text
    @@
    if List.length m.arguments > 0 then
      [ "{{"; m.name; "("; String.concat "," m.arguments; ")}}" ]
    else
      [ "{{"; m.name; "}}" ]
  else
    fst @@ macro_embed refs state config m

and macro_embed ?(outdent = false) refs state config { arguments; _ } =
  if List.length arguments <> 1 then
    ([], false)
  else
    let arg = String.trim (List.hd arguments) in
    let value = String.(trim @@ sub arg 2 (length arg - 4)) in
    let raw_result = map_raw_text [ "{{embed "; arg; "}}" ] in
    let current_level =
      if outdent then
        state.current_level - 1
      else
        state.current_level
    in
    if starts_with arg "[[" then
      (* page embed *)
      let pagename = value in
      if List.mem pagename state.embed_history then
        (raw_result, false)
      else
        match List.assoc_opt pagename refs.parsed_embed_pages with
        | Some ast ->
          let embed_page =
            blocks_aux refs
              { state with
                embed_parent_indent_level = current_level
              ; embed_history = pagename :: state.embed_history
              ; top_heading_level = None
              }
              config ast
          in
          (newline :: embed_page, true)
        | None -> (raw_result, false)
    else if starts_with arg "((" then
      (* block embed *)
      let block_uuid = value in
      if List.mem block_uuid state.embed_history then
        (raw_result, false)
      else
        match List.assoc_opt block_uuid refs.parsed_embed_blocks with
        | Some (ast, _) ->
          let embed_block =
            blocks_aux refs
              { state with
                embed_parent_indent_level = current_level
              ; embed_history = block_uuid :: state.embed_history
              ; top_heading_level = None
              }
              config ast
          in
          (newline :: embed_block, true)
        | None -> (raw_result, false)
    else
      (raw_result, false)

and entity { unicode; _ } = [ raw_text unicode ]

and timestamp t =
  map_raw_text
  @@
  match t with
  | Scheduled tt -> [ "SCHEDULED: "; Timestamp.to_string tt ]
  | Deadline tt -> [ "DEADLINE: "; Timestamp.to_string tt ]
  | Date tt -> [ Timestamp.to_string tt ]
  | Closed tt -> [ "CLOSED: "; Timestamp.to_string tt ]
  | Clock (Started tt) -> [ "CLOCK: "; Timestamp.to_string tt ]
  | Clock (Stopped rt) -> [ "CLOCK: "; Range.to_string rt ]
  | Range rt -> [ Range.to_string rt ]

and block_reference (refs : refs) state config block_uuid =
  match List.assoc_opt block_uuid refs.parsed_embed_blocks with
  | None -> map_raw_text [ "(("; block_uuid; "))" ]
  | Some (_, content) -> flatten_map (block refs state config) content

and block refs state config t =
  match t with
  | Paragraph l ->
    flatten_map (fun e -> Space :: inline refs state config e) l @ [ newline ]
  | Paragraph_line l -> raw_text_indent state config l @ [ newline ]
  | Paragraph_Sep n -> [ raw_text @@ String.make n '\n' ]
  | Heading h -> heading refs state config h
  | List l -> list refs state config l
  | Directive _ -> []
  | Results -> []
  | Example sl -> example state sl
  | Src cb -> src state config cb
  | Quote tl -> quote refs state config tl
  | Export _ -> []
  | CommentBlock _ -> []
  | Custom _ -> []
  | Latex_Fragment lf -> latex_fragment lf
  | Latex_Environment (name, options, content) ->
    latex_env state config name options content
  | Displayed_Math s ->
    [ Space; raw_text "$$"; raw_text s; raw_text "$$"; Space ]
  | Drawer (name, kvs) -> drawer state config name kvs
  | Property_Drawer kvs ->
    if config.exporting_keep_properties then
      drawer state config "PROPERTIES" kvs
    else
      (* hide Property_Drawers *)
      []
  | Footnote_Definition (name, content) ->
    footnote_definition refs state config name content
  | Horizontal_Rule -> [ newline; raw_text "---"; newline ]
  | Table t -> table refs state config t
  | Comment s ->
    List.flatten
      [ raw_text_indent state config "<!---"
      ; [ newline ]
      ; raw_text_indent state config s
      ; [ newline ]
      ; raw_text_indent state config "-->"
      ; [ newline ]
      ]
  | Raw_Html s -> raw_text_indent state config s @ [ newline ]
  | Hiccup s -> [ raw_text s; Space ]

and heading_merely_have_embed { title; marker; priority; _ } =
  match (title, marker, priority) with
  | [ Macro hd ], None, None when hd.name = "embed" -> Some hd
  | _ -> None

and heading refs state config h =
  let { title; tags; marker; level; priority; unordered; meta; _ } = h in
  let priority =
    match priority with
    | Some c -> "[#" ^ String.make 1 c ^ "]"
    | None -> ""
  in
  let marker =
    match marker with
    | Some s -> s ^ ""
    | None -> ""
  in
  let top_heading_level = Option.default level state.top_heading_level in
  let _ =
    if Option.is_none state.top_heading_level then
      state.top_heading_level <- Some level
  in
  let level' =
    if state.embed_parent_indent_level > 0 && top_heading_level >= 2 then
      state.embed_parent_indent_level + level - top_heading_level + 1
    else
      state.embed_parent_indent_level + level
  in
  state.current_level <- level';
  let f () =
    let heading_or_list =
      if config.heading_to_list then
        if unordered then
          [ Indent state.current_level; raw_text "-" ]
        else
          [ Indent (2 * state.current_level); raw_text "-" ]
      else
        [ raw_text @@ String.make level' '#' ]
    in
    heading_or_list
    @ [ Space; raw_text marker; Space; raw_text priority; Space ]
    @ flatten_map (fun e -> Space :: inline refs state config e) title
    @ [ Space
      ; (if List.length tags > 0 then
          raw_text @@ ":" ^ String.concat ":" tags ^ ":"
        else
          raw_text "")
      ; newline
      ]
  in
  let heading =
    match heading_merely_have_embed h with
    | Some embed ->
      (* this heading merely have one embed page(or block),
         so override it by embed page(or block) *)
      let r, succ = macro_embed ~outdent:true refs state config embed in
      if succ then
        r
      else
        f ()
    | None -> f ()
  in
  let properties =
    if config.exporting_keep_properties then
      drawer state config "PROPERTIES" meta.properties
    else
      (* hide Property_Drawers *)
      []
  in
  List.append heading properties

and list refs state config l =
  (fun l ->
    if List.length l > 0 then
      l @ [ TwoNewlines ]
    else
      l)
  @@ List.flatten
  @@ CCList.map
       (fun { content; items; number; name; checkbox; indent; _ } ->
         let name' = flatten_map (inline refs state config) name in
         let content' = flatten_map (block refs state config) content in
         (* Definition Lists content if name isn't empty  *)
         let content'' =
           if name' <> [] then
             List.flatten
             @@ CCList.map
                  (fun l ->
                    List.flatten
                      [ [ raw_text ": " ]
                      ; block refs state config l
                      ; [ newline ]
                      ])
                  content
           else
             content'
         in
         let name'' =
           if List.length name' > 0 then
             name' @ [ newline ]
           else
             []
         and number' =
           match number with
           | Some n -> raw_text @@ string_of_int n ^ ". "
           | None when List.length name = 0 -> raw_text "* "
           | None -> raw_text ""
         and checkbox' =
           match checkbox with
           | Some true -> raw_text "[X]"
           | Some false -> raw_text "[ ]"
           | None -> raw_text ""
         and indent' =
           raw_text @@ String.make ((2 * state.current_level) + indent) ' '
         and items' = list refs state config items in
         List.flatten
           [ [ indent' ]
           ; [ number' ]
           ; [ checkbox' ]
           ; [ Space ]
           ; name''
           ; content''
           ; [ newline ]
           ; items'
           ; [ newline ]
           ])
       l

and example state sl =
  flatten_map
    (fun l ->
      [ Indent (state.current_level + 1); RawText "    "; RawText l; newline ])
    sl

and src state config { lines; language; _ } =
  List.flatten
    [ [ Indent (state.current_level + 1)
      ; raw_text "```"
      ; Space
      ; raw_text @@ Option.default "" language
      ; newline
      ]
    ; flatten_map (raw_text_indent state config) lines
    ; [ Indent (state.current_level + 1); raw_text "```"; newline ]
    ]

and quote refs state config tl =
  let indent = Indent (state.current_level + 1) in
  flatten_map
    (fun l ->
      List.flatten
        [ [ indent; raw_text ">"; Space ]
        ; block refs state config l
        ; [ newline ]
        ])
    tl

and latex_env state config name options content =
  [ Indent (state.current_level + 1)
  ; raw_text @@ "\\begin{" ^ name ^ "}"
  ; raw_text @@ Option.default "" options
  ; newline
  ]
  @ raw_text_indent state config content
  @ [ newline
    ; Indent (state.current_level + 1)
    ; raw_text @@ "\\end{" ^ name ^ "}"
    ; newline
    ]

and drawer state config name kvs =
  if config.format != Conf.Org && name = "PROPERTIES" then
    flatten_map
      (fun (k, v) -> raw_text_indent state config (k ^ ":: " ^ v) @ [ newline ])
      kvs
  else
    List.flatten
      [ [ raw_text @@ ":" ^ name ^ ":"; newline ]
      ; flatten_map
          (fun (k, v) ->
            (raw_text_indent state config @@ ":" ^ k ^ ":")
            @ [ Space; raw_text v; newline ])
          kvs
      ; [ raw_text ":END:"; newline ]
      ]

and footnote_definition refs state config name content =
  let content' = flatten_map (inline refs state config) content in
  List.flatten
    [ [ raw_text @@ "[^" ^ name ^ "]"; Space ]; content'; [ newline ] ]

and table refs state config { header; groups; _ } =
  match header with
  | None -> []
  | Some header ->
    let separated_line =
      "|" ^ String.concat "|" (List.map (fun _ -> "---") header) ^ "|"
    in
    let header_line =
      List.flatten
        [ [ Indent (state.current_level + 1) ]
        ; flatten_map
            (fun col ->
              Space :: raw_text "|" :: Space
              :: flatten_map (inline refs state config) col)
            header
        ; [ Space; raw_text "|" ]
        ]
    in
    let group_lines =
      flatten_map
        (flatten_map (fun row ->
             List.flatten
               [ flatten_map
                   (fun col ->
                     Indent (state.current_level + 1)
                     :: Space :: raw_text "|" :: Space
                     :: flatten_map (inline refs state config) col)
                   row
               ; [ Space; raw_text "|"; newline ]
               ]))
        groups
    in
    List.flatten
      [ [ TwoNewlines ]
      ; header_line
      ; [ newline
        ; Indent (state.current_level + 1)
        ; raw_text separated_line
        ; newline
        ]
      ; group_lines
      ; [ newline ]
      ]

and blocks_aux refs state config tl =
  flatten_map (fun t -> Space :: block refs state config t) tl

let blocks refs config tl = blocks_aux refs (default_state ()) config tl

let directive kvs =
  if List.length kvs = 0 then
    []
  else
    let sep_line = [ newline; raw_text "---"; newline ] in
    sep_line
    @ flatten_map
        (fun (name, value) ->
          [ newline
          ; raw_text name
          ; raw_text ":"
          ; Space
          ; raw_text value
          ; newline
          ])
        kvs
    @ sep_line

(* 1.  [...;space; space]         -> [...;space]
   2.  [...;newline;newline]      -> [...;newline]
   3.  [...;space; newline]       -> [...;newline]
   4.  [...;newline;space]        -> [...;newline]
   5.  [...;"XXX\n";space]        -> [...;"XXX\n"]
   6.  [...;space;"\nXXX"]        -> [...;"\nXXX"]
   7.  [...;"XXX\n";newline]      -> [...;"XXX\n"]
   8.  [...;newline;"\nXXX"]      -> [...;"\nXXX"]
   9.  [...;"XXX<space>";space]   -> [...;"XXX<space>"]
   10. [...;space;"<space>XXX"]   -> [...;"<space>XXX"]
   11. [...;"XXX<space>";newline] -> [...;"XXX<space>";newline]
   12. [...;space;"XXX"]          -> [...;space;"XXX"]
   13. [...;newline;"XXX"]        -> [...;newline;"XXX"]
   14. [...;space;indent]         -> [...;indent]
   15. [...;indent;indent]        -> [...;indent]
   16. [...;newline;indent]       -> [...;newline;indent]
   17. [...;indent;space]         -> [...;indent]
   18. [...;indent;newline]       -> [...;newline]
   19. [...;"XXX\n";indent]       -> [...;"XXX\n";indent]
   20. [...;"XXX<space>";indent]  -> [...;"XXX<space>"]
   21. [...;indent;"XXX"]         -> [...;indent;"XXX"]
   22. [...;indent;"\nXXX"]       -> [...;"\nXXX"]
   23. [...;"XXX";indent]         -> [...;"XXX"] // XXX is not endwith '\n'
 * *)
let merge_adjacent_space_newline =
  let suffix_newline_num s =
    let len = String.length s in
    if len = 0 then
      None
    else if len = 1 && s.[0] = '\n' then
      Some 1
    else if len > 1 && s.[len - 1] = '\n' && s.[len - 2] = '\n' then
      Some 2
    else if len > 1 && s.[len - 1] = '\n' then
      Some 1
    else
      None
  in
  let to_t = function
    | `Space -> Space
    | `Newline -> Newline
    | `TwoNewlines -> TwoNewlines
    | `OneNewline -> OneNewline
    | `Indent n -> Indent n
  in
  fun tl ->
    List.rev
    @@ (fun (r, _, _, _, _) -> r)
    @@ List.fold_left
         (fun ( result
              , before
              , before_space_text
              , before_newline_text
              , start_of_line ) e ->
           match
             (e, before, before_space_text, before_newline_text, start_of_line)
           with
           | Space, _, _, Some _, _ ->
             (* 5 *) (result, [], false, before_newline_text, start_of_line)
           | Space, _, true, None, _ ->
             (* 9 *) (result, [], true, None, start_of_line)
           | Space, `Space :: _, false, None, _ ->
             (* 1 *)
             (result, [ `Space ], false, None, start_of_line)
           | Space, `Newline :: _, false, None, _ ->
             (* 4 *)
             (result, [ `Newline ], false, None, start_of_line)
           | Space, `Indent n :: _, false, None, _ ->
             (* 17 *)
             (result, [ `Indent n ], false, None, start_of_line)
           | Space, `OneNewline :: _, false, None, _ ->
             (result, [ `OneNewline ], false, None, start_of_line)
           | Space, `TwoNewlines :: _, false, None, _ ->
             (result, [ `TwoNewlines ], false, None, start_of_line)
           | Space, [], false, None, _ ->
             (* 12 *)
             (result, [ `Space ], false, None, start_of_line)
           | Newline, _, _, Some _, _ ->
             (*  7 *) (result, [], false, before_newline_text, true)
           | Newline, _, true, None, _ ->
             (* 11 *)
             (result, [ `Newline ], false, None, true)
           | Newline, `Space :: _, false, None, _ ->
             (* 3 *)
             (result, [ `Newline ], false, None, true)
           | Newline, `Newline :: _, false, None, _ ->
             (* 2 *)
             (result, [ `Newline ], false, None, true)
           | Newline, `Indent _ :: _, false, None, _ ->
             (* 18 *)
             (result, [ `Newline ], false, None, true)
           | Newline, `OneNewline :: _, false, None, _ ->
             (result, [ `OneNewline ], false, None, true)
           | Newline, `TwoNewlines :: _, false, None, _ ->
             (result, [ `TwoNewlines ], false, None, true)
           | Newline, [], false, None, _ ->
             (result, [ `Newline ], false, None, true)
           | OneNewline, _, _, Some _, _ ->
             (* before_newline_text can't erase OneNewline *)
             (result, [ `OneNewline ], false, None, true)
           | OneNewline, _, true, None, _ ->
             (result, [ `OneNewline ], false, None, true)
           | OneNewline, `Space :: _, false, None, _ ->
             (result, [ `OneNewline ], false, None, true)
           | OneNewline, `Newline :: _, false, None, _ ->
             (result, [ `OneNewline ], false, None, true)
           | OneNewline, `Indent _ :: _, false, None, _ ->
             (result, [ `OneNewline ], false, None, true)
           | OneNewline, `TwoNewlines :: _, false, None, _ ->
             (result, [ `TwoNewlines ], false, None, true)
           | OneNewline, `OneNewline :: _, false, None, _ ->
             (result, [ `OneNewline ], false, None, true)
           | OneNewline, [], false, None, _ ->
             (result, [ `OneNewline ], false, None, true)
           | TwoNewlines, _, _, Some 2, _ ->
             (result, [], false, before_newline_text, true)
           | TwoNewlines, _, _, Some _, _ ->
             (result, [ `OneNewline ], false, before_newline_text, true)
           | TwoNewlines, _, true, None, _ ->
             (result, [ `TwoNewlines ], false, None, true)
           | TwoNewlines, `Space :: _, false, None, _ ->
             (result, [ `TwoNewlines ], false, None, true)
           | TwoNewlines, `Newline :: _, false, None, _ ->
             (result, [ `TwoNewlines ], false, None, true)
           | TwoNewlines, `Indent _ :: _, false, None, _ ->
             (result, [ `TwoNewlines ], false, None, true)
           | TwoNewlines, `OneNewline :: _, false, None, _ ->
             (result, [ `TwoNewlines ], false, None, true)
           | TwoNewlines, `TwoNewlines :: _, false, None, _ ->
             (result, [ `TwoNewlines ], false, None, true)
           | TwoNewlines, [], false, None, _ ->
             (result, [ `TwoNewlines ], false, None, true)
           | Indent 0, a, b, c, _ ->
             (* ignore *)
             (result, a, b, c, start_of_line)
           | Indent n, before, _, _, true ->
             ( CCList.append (CCList.map to_t before) result
             , [ `Indent n ]
             , false
             , None
             , false )
           | Indent n, _, _, Some _, _ ->
             (* 19 *)
             (result, [ `Indent n ], false, None, start_of_line)
           | Indent _, _, true, None, false ->
             (* 20 *)
             (result, before, true, None, start_of_line)
           | Indent _, `Space :: _, false, None, false ->
             (* 14 *)
             (result, [ `Space ], false, None, start_of_line)
           | Indent n, `Indent _ :: _, false, None, false ->
             (* 15 *)
             (result, [ `Indent n ], false, None, start_of_line)
           | Indent n, `Newline :: _, false, None, false ->
             (* 16 *)
             (Newline :: result, [ `Indent n ], false, None, true)
           | Indent n, `OneNewline :: _, false, None, false ->
             (OneNewline :: result, [ `Indent n ], false, None, true)
           | Indent n, `TwoNewlines :: _, false, None, false ->
             (TwoNewlines :: result, [ `Indent n ], false, None, true)
           | Indent _, [], false, None, false ->
             (* 23 *)
             (result, [], false, None, false)
           | RawText "", _, _, _, _ ->
             ( result
             , before
             , before_space_text
             , before_newline_text
             , start_of_line )
           | RawText s, `Space :: _, _, _, _ when s.[0] = '\n' || s.[0] = ' ' ->
             (* 6,10 *)
             let last_char = s.[String.length s - 1] in
             ( e :: result
             , []
             , last_char = ' '
             , suffix_newline_num s
             , last_char = '\n' )
           | RawText s, `Space :: _, _, _, _ ->
             (* 12 *)
             let last_char = s.[String.length s - 1] in
             ( e :: Space :: result
             , []
             , last_char = ' '
             , suffix_newline_num s
             , last_char = '\n' )
           | RawText s, `Newline :: _, _, _, _ when s.[0] = '\n' ->
             (* 8 *)
             let last_char = s.[String.length s - 1] in
             ( e :: result
             , []
             , last_char = ' '
             , suffix_newline_num s
             , last_char = '\n' )
           | RawText s, `Newline :: _, _, _, _ ->
             (* 13 *)
             let last_char = s.[String.length s - 1] in
             ( e :: Newline :: result
             , []
             , last_char = ' '
             , suffix_newline_num s
             , last_char = '\n' )
           | RawText s, `OneNewline :: _, _, _, _ when s.[0] = '\n' ->
             let last_char = s.[String.length s - 1] in
             ( e :: result
             , []
             , last_char = ' '
             , suffix_newline_num s
             , last_char = '\n' )
           | RawText s, `OneNewline :: _, _, _, _ ->
             let last_char = s.[String.length s - 1] in
             ( e :: OneNewline :: result
             , []
             , last_char = ' '
             , suffix_newline_num s
             , last_char = '\n' )
           | RawText s, `TwoNewlines :: _, _, _, _ ->
             let result =
               if String.length s = 1 then
                 if s.[0] = '\n' then
                   TwoNewlines :: result
                 else
                   e :: TwoNewlines :: result
               else if s.[0] = '\n' && s.[1] = '\n' then
                 e :: result
               else if s.[0] = '\n' then
                 RawText (String.sub s 1 (String.length s - 1))
                 :: TwoNewlines :: result
               else
                 e :: TwoNewlines :: result
             in
             let last_char = s.[String.length s - 1] in
             ( result
             , []
             , last_char = ' '
             , suffix_newline_num s
             , last_char = '\n' )
           | RawText s, `Indent _ :: _, _, _, _ when s.[0] = '\n' ->
             (* 22 *)
             let last_char = s.[String.length s - 1] in
             ( e :: result
             , []
             , last_char = ' '
             , suffix_newline_num s
             , last_char = '\n' )
           | RawText s, `Indent n :: _, _, _, _ ->
             (* 21 *)
             let last_char = s.[String.length s - 1] in
             ( e :: Indent n :: result
             , []
             , last_char = ' '
             , suffix_newline_num s
             , last_char = '\n' )
           | RawText s, [], _, _, _ ->
             let last_char = s.[String.length s - 1] in
             ( e :: result
             , []
             , last_char = ' '
             , suffix_newline_num s
             , last_char = '\n' ))
         ([], [ `Space ], false, None, true)
         tl

let remove_fst_space_newline = function
  | [] -> []
  | Space :: t -> t
  | Newline :: t -> t
  | l -> l

let remove_greatest_common_prefix_indent config tl =
  if not config.heading_to_list then
    tl
  else
    let find_greatest_common_prefix_indent tl =
      List.fold_left
        (fun r e ->
          match e with
          | Indent n when n < r -> n
          | _ -> r)
        999 tl
    in
    let prefix = find_greatest_common_prefix_indent tl in
    CCList.map
      (fun e ->
        match e with
        | Indent n when n < prefix -> Indent 0
        | Indent n -> Indent (n - prefix)
        | _ -> e)
      tl

let to_string config tl =
  String.concat ""
  @@ CCList.map
       (function
         | Space -> " "
         | Newline -> "\n"
         | TwoNewlines -> "\n\n"
         | OneNewline -> "\n"
         | Indent n ->
           if config.heading_to_list then
             String.make n ' '
           else
             ""
         | RawText s -> s)
       (remove_fst_space_newline
          (remove_greatest_common_prefix_indent config
             (merge_adjacent_space_newline (merge_adjacent_space_newline tl))))

module MarkdownExporter = struct
  let name = "markdown"

  let default_filename = change_ext "md"

  let export ~refs config doc output =
    let refs =
      Option.default
        Reference.{ parsed_embed_blocks = []; parsed_embed_pages = [] }
        refs
    in
    let doc_blocks = CCList.map fst doc.blocks in
    let directives = directive doc.directives in
    let blocks = blocks refs config doc_blocks in
    output_string output (to_string config (CCList.append directives blocks))
end

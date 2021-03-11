open Prelude
open Type
open Inline
open Document
open Reference

type t =
  | RawText of string
  (* merge adjacent multiple Space as one Space *)
  | Space
  (* merge adjacent multiple Newline as one Newline *)
  | Newline

let show t =
  match t with
  | RawText s -> "RawText:\"" ^ s ^ "\""
  | Space -> "Space"
  | Newline -> "Newline"

let raw_text s = RawText s

let map_raw_text = List.map raw_text

let flatten_map f l = List.flatten (CCList.map f l)

type refs = Reference.parsed_t

let src_block ?(lang = None) ?(options = None) typ sl =
  let options =
    match options with
    | None -> ""
    | Some v -> String.concat " " v
  in
  List.flatten
    [ [ raw_text @@ "#+BEGIN_" ^ typ
      ; Space
      ; raw_text @@ Option.default "" lang
      ; Space
      ; raw_text options
      ; Newline
      ]
    ; sl
    ; [ Newline; raw_text @@ "#END_" ^ typ; Newline ]
    ]

type state =
  { outside_em_symbol : char option
  ; embed_history : string list
  ; embed_parent_indent_level : int
  ; mutable current_level : int
  }

let default_state () =
  { outside_em_symbol = None
  ; embed_parent_indent_level = 0
  ; current_level = 0
  ; embed_history = []
  }

let default_config = None

let rec inline refs state config (t : Inline.t) : t list =
  match t with
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
      [ "{{{"; m.name; "("; String.concat "," m.arguments; ")}}}" ]
    else
      [ "{{{"; m.name; "}}}" ]
  else
    macro_embed refs state config m

and macro_embed refs state config { arguments; _ } =
  if List.length arguments <> 1 then
    []
  else
    let arg = String.trim (List.hd arguments) in
    let value = String.(trim @@ sub arg 2 (length arg - 4)) in
    let raw_result = map_raw_text [ "{{{embed "; arg; "}}}" ] in
    let current_level = state.current_level in
    if starts_with arg "[[" then
      (* page embed *)
      let pagename = value in
      if List.mem pagename state.embed_history then
        raw_result
      else
        match List.assoc_opt pagename refs.parsed_embed_pages with
        | Some ast ->
          let embed_page =
            blocks_aux refs
              { state with
                embed_parent_indent_level = current_level
              ; embed_history = pagename :: state.embed_history
              }
              config ast
          in
          Newline :: embed_page
        | None -> raw_result
    else if starts_with arg "((" then
      (* block embed *)
      let block_uuid = value in
      if List.mem block_uuid state.embed_history then
        raw_result
      else
        match List.assoc_opt block_uuid refs.parsed_embed_blocks with
        | Some (ast, _) ->
          let embed_block =
            blocks_aux refs
              { state with
                embed_parent_indent_level = current_level
              ; embed_history = block_uuid :: state.embed_history
              }
              config ast
          in
          Newline :: embed_block
        | None -> raw_result
    else
      raw_result

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
  | Some (_, title) ->
    let title = flatten_map (inline refs state config) title in
    List.flatten [ [ raw_text "((" ]; title; [ raw_text "))" ] ]

and block refs state config t =
  match t with
  | Paragraph l ->
    flatten_map (fun e -> Space :: inline refs state config e) l @ [ Newline ]
  | Paragraph_line l -> [ raw_text l; Newline ]
  | Paragraph_Sep n -> [ raw_text @@ String.make n '\n' ]
  | Heading h -> heading refs state config h
  | List l -> list refs state config l
  | Directive _ -> []
  | Results -> []
  | Example sl -> example sl
  | Src cb -> src cb
  | Quote tl -> quote refs state config tl
  | Export (name, options, content) ->
    src_block ~lang:(Some name) ~options "EXPORT" [ raw_text content ]
  | CommentBlock sl -> src_block "COMMENT" (map_raw_text sl)
  | Custom (typ, options, _, content) ->
    src_block typ
      ~options:(Option.map (fun v -> [ v ]) options)
      [ raw_text content ]
  | Latex_Fragment lf -> latex_fragment lf
  | Latex_Environment (name, options, content) -> latex_env name options content
  | Displayed_Math s ->
    [ Space; raw_text "$$"; raw_text s; raw_text "$$"; Space ]
  | Drawer (name, kvs) -> drawer name kvs
  | Property_Drawer _ ->
    (* hide Property_Drawers *)
    []
  | Footnote_Definition (name, content) ->
    footnote_definition refs state config name content
  | Horizontal_Rule -> [ raw_text "---"; Newline ]
  | Table t -> table refs state config t
  | Comment s ->
    [ raw_text "<!---"; Newline; raw_text s; Newline; raw_text "-->"; Newline ]
  | Raw_Html s -> [ raw_text s; Newline ]
  | Hiccup s -> [ raw_text s; Space ]

and heading refs state config { title; tags; marker; level; priority; _ } =
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
  let level' = state.embed_parent_indent_level + level in
  state.current_level <- level';
  [ raw_text @@ String.make level' '#'
  ; Space
  ; raw_text marker
  ; Space
  ; raw_text priority
  ; Space
  ]
  @ flatten_map (fun e -> Space :: inline refs state config e) title
  @ [ Space
    ; (if List.length tags > 0 then
        raw_text @@ ":" ^ String.concat ":" tags ^ ":"
      else
        raw_text "")
    ; Newline
    ]

and list refs state config l =
  List.flatten
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
                      ; [ Newline ]
                      ])
                  content
           else
             content'
         in
         let name'' =
           if List.length name' > 0 then
             name' @ [ Newline ]
           else
             []
         and number' =
           match number with
           | Some n -> raw_text @@ string_of_int n ^ ". "
           | None when List.length name = 0 -> raw_text "- "
           | None -> raw_text ""
         and checkbox' =
           match checkbox with
           | Some true -> raw_text "[X]"
           | Some false -> raw_text "[ ]"
           | None -> raw_text ""
         and indent' = raw_text @@ String.make indent ' '
         and items' = list refs state config items in
         List.flatten
           [ [ indent' ]
           ; [ number' ]
           ; [ checkbox' ]
           ; [ Space ]
           ; name''
           ; content''
           ; [ Newline ]
           ; items'
           ; [ Newline ]
           ])
       l

and example sl =
  flatten_map (fun l -> [ RawText "    "; RawText l; Newline ]) sl

and src { lines; language; _ } =
  List.flatten
    [ [ raw_text "```"; Space; raw_text @@ Option.default "" language; Newline ]
    ; map_raw_text lines
    ; [ raw_text "```"; Newline ]
    ]

and quote refs state config tl =
  flatten_map
    (fun l ->
      List.flatten
        [ [ RawText ">"; Space ]; block refs state config l; [ Newline ] ])
    tl

and latex_env name options content =
  [ raw_text @@ "\\begin{" ^ name ^ "}"
  ; raw_text @@ Option.default "" options
  ; Newline
  ; raw_text content
  ; Newline
  ; raw_text @@ "\\end{" ^ name ^ "}"
  ; Newline
  ]

and drawer name kvs =
  List.flatten
    [ [ raw_text @@ ":" ^ name ^ ":"; Newline ]
    ; List.flatten
      @@ CCList.map
           (fun (k, v) -> [ raw_text @@ ":" ^ k ^ ":"; Space; raw_text v ])
           kvs
    ; [ Newline; raw_text ":END:"; Newline ]
    ]

and footnote_definition refs state config name content =
  let content' = flatten_map (inline refs state config) content in
  List.flatten
    [ [ raw_text @@ "[^" ^ name ^ "]"; Space ]; content'; [ Newline ] ]

and table refs state config { header; groups; _ } =
  match header with
  | None -> []
  | Some header ->
    let separated_line =
      "|" ^ String.concat "|" (List.map (fun _ -> "---") header) ^ "|"
    in
    let header_line =
      List.flatten
        [ flatten_map
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
                     Space :: raw_text "|" :: Space
                     :: flatten_map (inline refs state config) col)
                   row
               ; [ Space; raw_text "|"; Newline ]
               ]))
        groups
    in
    List.flatten
      [ header_line
      ; [ Newline; raw_text separated_line; Newline ]
      ; group_lines
      ; [ Newline ]
      ]

and blocks_aux refs state config tl =
  flatten_map (fun t -> Space :: block refs state config t) tl

let blocks refs config tl = blocks_aux refs (default_state ()) config tl

let directive kvs =
  let sep_line = [ Newline; raw_text "---"; Newline ] in
  sep_line
  @ flatten_map
      (fun (name, value) ->
        [ Newline; raw_text name; raw_text ":"; Space; raw_text value; Newline ])
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
**)
let merge_adjacent_space_newline tl =
  List.rev
  @@ (fun (r, _, _, _) -> r)
  @@ List.fold_left
       (fun (result, before, before_space_text, before_newline_text) e ->
         match (e, before, before_space_text, before_newline_text) with
         | Space, _, _, true -> (* 5 *) (result, None, false, true)
         | Space, _, true, false -> (* 9 *) (result, None, true, false)
         | Space, Some `Space, false, false ->
           (* 1 *)
           (result, Some `Space, false, false)
         | Space, Some `Newline, false, false ->
           (* 4 *)
           (result, Some `Newline, false, false)
         | Space, None, false, false ->
           (* 12 *)
           (result, Some `Space, false, false)
         | Newline, _, _, true -> (*  7 *) (result, None, false, true)
         | Newline, _, true, false ->
           (* 11 *)
           (result, Some `Newline, false, false)
         | Newline, Some `Space, false, false ->
           (* 3 *)
           (result, Some `Newline, false, false)
         | Newline, Some `Newline, false, false ->
           (* 2 *)
           (result, Some `Newline, false, false)
         | Newline, None, false, false -> (result, Some `Newline, false, false)
         | RawText "", _, _, _ ->
           (result, before, before_space_text, before_newline_text)
         | RawText s, Some `Space, _, _ when s.[0] = '\n' || s.[0] = ' ' ->
           (* 6,10 *)
           let last_char = s.[String.length s - 1] in
           (e :: result, None, last_char = ' ', last_char = '\n')
         | RawText s, Some `Space, _, _ ->
           (* 12 *)
           let last_char = s.[String.length s - 1] in
           (e :: Space :: result, None, last_char = ' ', last_char = '\n')
         | RawText s, Some `Newline, _, _ when s.[0] = '\n' ->
           (* 8 *)
           let last_char = s.[String.length s - 1] in
           (e :: result, None, last_char = ' ', last_char = '\n')
         | RawText s, Some `Newline, _, _ ->
           (* 13 *)
           let last_char = s.[String.length s - 1] in
           (e :: Newline :: result, None, last_char = ' ', last_char = '\n')
         | RawText s, None, _, _ ->
           let last_char = s.[String.length s - 1] in
           (e :: result, None, last_char = ' ', last_char = '\n'))
       ([], Some `Space, false, false)
       tl

let remove_fst_space_newline = function
  | [] -> []
  | Space :: t -> t
  | Newline :: t -> t
  | l -> l

let to_string tl =
  String.concat ""
  @@ CCList.map
       (function
         | Space -> " "
         | Newline -> "\n"
         | RawText s -> s)
       (remove_fst_space_newline (merge_adjacent_space_newline tl))

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
    output_string output (to_string (CCList.append directives blocks))
end

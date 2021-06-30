open Prelude
open Type
open Inline
open Document
open Conf
open Output

type t = Output.t

let show t =
  match t with
  | RawText s -> "RawText:\"" ^ s ^ "\""
  | Space -> "Space"
  | Newline -> "Newline"
  | TwoNewlines -> "TwoNewlines"
  | OneNewline -> "OneNewline"
  | Indent (n1, n2) -> Printf.sprintf "Indent(%d, %d)" n1 n2

let raw_text s = RawText s

let newline = Newline

let map_raw_text = List.map raw_text

let flatten_map f l = List.flatten (List.map f l)

type refs = Reference.parsed_t

type state =
  { outside_em_symbol : char option
  ; embed_history : string list
  ; embed_parent_indent_level : int
  ; current_level : int (* ; mutable top_heading_level : int option *)
  ; mutable last_newline : bool
  }

let default_state () =
  { outside_em_symbol = None
  ; embed_parent_indent_level = 0
  ; current_level = -2
  ; embed_history = []
  ; last_newline = false
  }

let indent_with_2_spacemore n = Indent (n, 2)

(* let indent_with_2_spacemore_if_start_of_line s =  *)

let raw_text_indent state config s =
  let indent state config s =
    if config.heading_to_list then
      let ls =
        lines s
        |> flatten_map (fun l ->
               [ indent_with_2_spacemore state.current_level
               ; raw_text (l ^ "\n")
               ])
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
    indent_with_2_spacemore state.current_level :: indent state config s

let rec inline state config (t : Inline.t) : t list =
  let content =
    indent_with_2_spacemore state.current_level
    ::
    (match t with
    | Emphasis em -> emphasis state config em
    | Break_Line ->
      state.last_newline <- true;
      [ raw_text "\n" ]
    | Hard_Break_Line ->
      state.last_newline <- true;
      [ raw_text "  \n" ]
    | Verbatim s -> [ raw_text s ]
    | Code s -> map_raw_text [ "`"; s; "`" ] (* it's inline code *)
    | Tag s -> map_raw_text [ "#"; Inline.hash_tag_value_string (Tag s) ]
    | Spaces s ->
      if state.last_newline then
        []
      else
        map_raw_text [ s ]
    | Plain s ->
      if state.last_newline then (
        state.last_newline <- false;
        map_raw_text [ String.ltrim s ]
      ) else
        map_raw_text [ s ]
    | Link l -> inline_link l
    | Nested_link l -> inline_nested_link @@ fst l
    | Target s -> map_raw_text [ "<<"; s; ">>" ]
    | Subscript tl -> inline_subscript state config tl
    | Superscript tl -> inline_superscript state config tl
    | Footnote_Reference fr -> footnote_reference fr
    | Cookie c -> cookie c
    | Latex_Fragment lf -> latex_fragment lf
    | Macro m -> macro m
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
    | Inline_Hiccup s -> map_raw_text [ s ]
    | Inline_Html s -> map_raw_text [ s ])
  in
  let _ =
    match t with
    | Break_Line
    | Hard_Break_Line
    | Spaces _ ->
      ()
    | _ -> state.last_newline <- false
  in
  content

and emphasis state config (typ, tl) =
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
        @@ List.map (inline { state with outside_em_symbol } config) tl
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
    @@ List.map
         (fun e -> Space :: inline { state with outside_em_symbol } config e)
         tl

and inline_link { full_text; _ } = [ raw_text full_text ]

and inline_nested_link { content; _ } = [ raw_text content ]

and inline_subscript state config tl =
  List.flatten
    [ [ raw_text "_{" ]
    ; flatten_map (fun e -> Space :: inline state config e) tl
    ; [ raw_text "}" ]
    ]

and inline_superscript state config tl =
  List.flatten
    [ [ raw_text "^{" ]
    ; flatten_map (fun e -> Space :: inline state config e) tl
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

and macro m =
  map_raw_text
  @@
  if List.length m.arguments > 0 then
    [ "{{"; m.name; "("; String.concat "," m.arguments; ")}}" ]
  else
    [ "{{"; m.name; "}}" ]

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

and block_reference block_uuid = map_raw_text [ "(("; block_uuid; "))" ]

and block state config t =
  let content =
    match t with
    | Paragraph l ->
      flatten_map
        (fun e -> Space :: inline state config e)
        (Type_op.inline_list_strip_pos l)
      @ [ newline ]
    | Paragraph_line l -> raw_text_indent state config l @ [ newline ]
    | Paragraph_Sep n -> [ raw_text @@ String.make n '\n' ]
    | Heading h -> heading state config h
    | List l -> list state config l
    | Directive _ -> []
    | Results -> []
    | Example sl -> example state sl
    | Src cb -> src state config cb
    | Quote tl -> quote state config tl
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
      footnote_definition state config name content
    | Horizontal_Rule -> [ newline; raw_text "---"; newline ]
    | Table t -> table state config t
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
  in
  state.last_newline <- true;
  content

and heading_merely_have_embed { title; marker; priority; _ } =
  match (title, marker, priority) with
  | [ (Macro hd, _) ], None, None when hd.name = "embed" -> Some hd
  | _ -> None

and heading state config h =
  let { title; tags; marker; priority; meta; unordered; level; _ } = h in
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
  let f () =
    let heading_or_list =
      match (config.heading_to_list, config.ignore_heading_list_marker) with
      | true, false -> [ Indent (state.current_level, 0); raw_text "-" ]
      | true, true -> [ Indent (state.current_level, 0) ]
      | false, _ ->
        if unordered || level <= 0 then
          [ Indent (state.current_level, 0); raw_text "-" ]
        else
          [ raw_text @@ String.make level '#' ]
    in
    heading_or_list
    @ [ Space; raw_text marker; Space; raw_text priority; Space ]
    @ flatten_map (fun e -> Space :: inline state config e) (List.map fst title)
    @ [ Space
      ; (if List.length tags > 0 then
          raw_text @@ ":" ^ String.concat ":" tags ^ ":"
        else
          raw_text "")
      ; newline
      ]
  in
  let heading = f () in
  let properties =
    if config.exporting_keep_properties then
      drawer state config "PROPERTIES" meta.properties
    else
      (* hide Property_Drawers *)
      []
  in
  List.append heading properties

and list state config l =
  (fun l ->
    if List.length l > 0 then
      l @ [ TwoNewlines ]
    else
      l)
  @@ List.flatten
  @@ List.map
       (fun { content; items; number; name; checkbox; _ } ->
         let state' = { state with current_level = state.current_level + 1 } in
         let name' =
           flatten_map (inline state config)
             (Type_op.inline_list_strip_pos name)
         in
         let content' = flatten_map (block state' config) content in
         (* Definition Lists content if name isn't empty  *)
         let content'' =
           if name' <> [] then
             List.flatten
             @@ List.map
                  (fun l ->
                    List.flatten
                      [ [ raw_text ": " ]; block state' config l; [ newline ] ])
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
           if state'.current_level > 0 then
             raw_text @@ String.make state'.current_level '\t'
           else
             raw_text ""
         and items' = list state' config items in
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
      [ indent_with_2_spacemore state.current_level
      ; RawText "    "
      ; RawText l
      ; newline
      ])
    sl

and src state config { lines; language; _ } =
  List.flatten
    [ [ indent_with_2_spacemore state.current_level
      ; raw_text "```"
      ; Space
      ; raw_text @@ Option.default "" language
      ; newline
      ]
    ; flatten_map (raw_text_indent state config) lines
    ; [ indent_with_2_spacemore state.current_level; raw_text "```"; newline ]
    ]

and quote state config tl =
  flatten_map
    (fun l ->
      List.flatten
        [ [ indent_with_2_spacemore state.current_level; raw_text ">"; Space ]
        ; block state config l
        ; [ newline ]
        ])
    tl

and latex_env state config name options content =
  [ indent_with_2_spacemore state.current_level
  ; raw_text @@ "\\begin{" ^ name ^ "}"
  ; raw_text @@ Option.default "" options
  ; newline
  ]
  @ raw_text_indent state config content
  @ [ newline
    ; indent_with_2_spacemore state.current_level
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

and footnote_definition state config name content =
  let content' =
    flatten_map (inline state config) (Type_op.inline_list_strip_pos content)
  in
  List.flatten
    [ [ raw_text @@ "[^" ^ name ^ "]"; Space ]; content'; [ newline ] ]

and table state config { header; groups; _ } =
  match header with
  | None -> []
  | Some header ->
    let separated_line =
      "|" ^ String.concat "|" (List.map (fun _ -> "---") header) ^ "|"
    in
    let header_line =
      List.flatten
        [ [ indent_with_2_spacemore state.current_level ]
        ; flatten_map
            (fun col ->
              Space :: raw_text "|" :: Space
              :: flatten_map (inline state config) col)
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
                     indent_with_2_spacemore state.current_level
                     :: Space :: raw_text "|" :: Space
                     :: flatten_map (inline state config) col)
                   row
               ; [ Space; raw_text "|"; newline ]
               ]))
        groups
    in
    List.flatten
      [ [ TwoNewlines ]
      ; header_line
      ; [ newline
        ; indent_with_2_spacemore state.current_level
        ; raw_text separated_line
        ; newline
        ]
      ; group_lines
      ; [ newline ]
      ]

let rec blocks_aux state config (v : Tree_type.value) =
  let open Zip in
  match v with
  | Leaf (b, _) -> block state config b
  | Branch [] -> []
  | Branch (Leaf (h, _) :: t) ->
    let state' = { state with current_level = state.current_level + 1 } in
    let heading = block state' config h in
    List.flatten @@ (heading :: List.map (blocks_aux state' config) t)
  | Branch l ->
    let state' = { state with current_level = state.current_level + 1 } in
    List.flatten @@ List.map (blocks_aux state' config) l

let blocks refs config tl =
  let open Tree_type in
  let z = of_blocks tl in
  let z' = replace_embed_and_refs z ~refs in
  let v = to_value z' in
  blocks_aux (default_state ()) config v

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

module String_Tree_Value = struct
  type t = string Zip.l

  let rec of_value v ~config =
    match v with
    | Zip.Leaf (t, _pos) ->
      Zip.leaf @@ Output.to_string @@ block (default_state ()) config t
    | Zip.Branch [] -> Zip.Branch []
    | Zip.Branch l -> Zip.branch @@ List.map (of_value ~config) l
end

module MarkdownExporter = struct
  let name = "markdown"

  let default_filename = change_ext "md"

  let export ~refs config doc output =
    let refs =
      Option.default
        Reference.{ parsed_embed_blocks = []; parsed_embed_pages = [] }
        refs
    in
    let directives = directive doc.directives in
    let blocks = blocks refs config doc.blocks in
    output_string output (to_string (List.append directives blocks))
end

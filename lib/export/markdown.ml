open Prelude
open Type
open Inline

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

type state = { outside_em_symbol : char option }

let default_state = { outside_em_symbol = None }

let default_config = None

let rec inline state config (t : Inline.t) : t list =
  match t with
  | Emphasis em -> emphasis state config em
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
  | Block_reference s -> map_raw_text [ "(("; s; "))" ]
  | Inline_Hiccup s -> map_raw_text [ s ]

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
        @@ CCList.map
             (fun e -> Space :: inline { outside_em_symbol } config e)
             tl
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
    @@ CCList.map (fun e -> Space :: inline { outside_em_symbol } config e) tl

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
  | Inline s -> map_raw_text [ "$"; s; "$" ]
  | Displayed s -> map_raw_text [ "$$"; s; "$$" ]

and macro { name; arguments } =
  map_raw_text
  @@
  if List.length arguments > 0 then
    [ "{{{"; name; "("; String.concat "," arguments; ")}}}" ]
  else
    [ "{{{"; name; "}}}" ]

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

let rec block state config t =
  match t with
  | Paragraph l ->
    flatten_map (fun e -> Space :: inline state config e) l @ [ Newline ]
  | Paragraph_line l -> [ raw_text l; Newline ]
  | Paragraph_Sep n -> [ raw_text @@ String.make n '\n' ]
  | Heading h -> heading state config h
  | List l -> list state config l
  | Directive (name, value) -> directive name value
  | Results -> []
  | Example sl -> src_block "EXAMPLE" (map_raw_text sl)
  | Src cb -> src cb
  | Quote tl -> quote state config tl
  | Export (name, options, content) ->
    src_block ~lang:(Some name) ~options "EXPORT" [ raw_text content ]
  | CommentBlock sl -> src_block "COMMENT" (map_raw_text sl)
  | Custom (typ, options, _, content) ->
    src_block typ
      ~options:(Option.map (fun v -> [ v ]) options)
      [ raw_text content ]
  | Latex_Fragment lf -> latex_fragment lf
  | Latex_Environment (name, options, content) -> latex_env name options content
  | Displayed_Math s -> map_raw_text [ "$$"; s; "$$" ]
  | Drawer (name, kvs) -> drawer name kvs
  | Property_Drawer kvs -> drawer "PROPERTIES" kvs
  | Footnote_Definition (name, content) ->
    footnote_definition state config name content
  | Horizontal_Rule -> [ raw_text "---" ]
  | Table t -> table state config t
  | Comment s ->
    [ raw_text "<!---"; Newline; raw_text s; Newline; raw_text "-->" ]
  | Raw_Html s -> [ raw_text s ]
  | Hiccup s -> [ raw_text s ]

and heading state config { title; tags; marker; level; priority; _ } =
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
  [ raw_text @@ String.make level '#'
  ; Space
  ; raw_text marker
  ; Space
  ; raw_text priority
  ; Space
  ]
  @ flatten_map (fun e -> Space :: inline state config e) title
  @ [ Space
    ; (if List.length tags > 0 then
        raw_text @@ ":" ^ String.concat ":" tags ^ ":"
      else
        raw_text "")
    ]

and list state config l =
  List.flatten
  @@ CCList.map
       (fun { content; items; number; name; checkbox; indent; _ } ->
         let name' = flatten_map (inline state config) name in
         let content' = flatten_map (block state config) content in
         (* Definition Lists content if name isn't empty  *)
         let content'' =
           if name' <> [] then
             List.flatten
             @@ CCList.map
                  (fun l ->
                    List.flatten
                      [ [ raw_text ": " ]; block state config l; [ Newline ] ])
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
         and indent' = raw_text @@ String.make indent '\t'
         and items' = list state config items in
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

and directive name value =
  [ raw_text "#+"; raw_text name; raw_text ":"; Space; raw_text value ]

and src { lines; language; _ } =
  List.flatten
    [ [ raw_text "```"; Space; raw_text @@ Option.default "" language; Newline ]
    ; map_raw_text lines
    ; [ raw_text "```" ]
    ]

and quote state config tl =
  let content = flatten_map (block state config) tl in
  src_block "QUOTE" content

and latex_env name options content =
  [ raw_text @@ "\\begin{" ^ name ^ "}"
  ; raw_text @@ Option.default "" options
  ; Newline
  ; raw_text content
  ; Newline
  ; raw_text @@ "\\end{" ^ name ^ "}"
  ]

and drawer name kvs =
  List.flatten
    [ [ raw_text @@ ":" ^ name ^ ":"; Newline ]
    ; List.flatten
      @@ CCList.map
           (fun (k, v) -> [ raw_text @@ ":" ^ k ^ ":"; Space; raw_text v ])
           kvs
    ; [ Newline; raw_text ":END:" ]
    ]

and footnote_definition state config name content =
  let content' = flatten_map (inline state config) content in
  List.flatten [ [ raw_text @@ "[^" ^ name ^ "]"; Space ]; content' ]

and table state config { header; groups; _ } =
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
                     Space :: raw_text "|" :: Space
                     :: flatten_map (inline state config) col)
                   row
               ; [ Space; raw_text "|"; Newline ]
               ]))
        groups
    in
    List.flatten
      [ header_line
      ; [ Newline; raw_text separated_line; Newline ]
      ; group_lines
      ]

let blocks config tl =
  flatten_map (fun t -> Space :: block default_state config t) tl

let to_string tl =
  String.concat "" @@ List.rev
  @@ (fun (r, _, _) -> r)
  @@ List.fold_left
       (fun (result, prefix_space, prefix_newline) t ->
         (* Printf.printf "%s, %b, %b\n" (show t) prefix_space prefix_newline; *)
         match (t, prefix_space, prefix_newline) with
         | RawText "", _, _ -> (result, prefix_space, prefix_newline)
         | RawText s, false, false ->
           ( s :: result
           , s.[String.length s - 1] = ' '
           , s.[String.length s - 1] = '\n' )
         | RawText s, true, false ->
           let s_h, s_t = (s.[0], String.sub s 1 (String.length s - 1)) in
           if s_h = ' ' then
             (s_t :: result, s.[String.length s - 1] = ' ', false)
           else
             (s :: result, s.[String.length s - 1] = ' ', false)
         | RawText s, _, true ->
           let s_h, s_t = (s.[0], String.sub s 1 (String.length s - 1)) in
           if s_h = '\n' then
             ( s_t :: result
             , s.[String.length s - 1] = ' '
             , s.[String.length s - 1] = '\n' )
           else
             ( s :: result
             , s.[String.length s - 1] = ' '
             , s.[String.length s - 1] = '\n' )
         | Space, false, false when result = [] -> (result, true, false)
         | Space, false, false -> (" " :: result, true, false)
         | Space, true, false -> (result, true, false)
         | Space, _, true -> (result, true, true)
         | Newline, _, false -> ("\n" :: result, prefix_space, true)
         | Newline, _, true -> (result, prefix_space, true))
       ([], false, false) tl

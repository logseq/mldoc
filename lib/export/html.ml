open! Prelude
open Type
open Inline
open Document
open Timestamp
open Conf

(* taken from mlorg *)

let macros = ref []

(* #+OPTIONS:   num:nil toc:2 *)
let options = ref []

let concatmap f l = List.concat (List.map f l)

let list_element = function
  | [] -> "ul"
  | { ordered; name; _ } :: _tl ->
    let name =
      if List.length name == 0 then
        false
      else
        true
    in
    if name then
      "dl"
    else if ordered then
      "ol"
    else
      "ul"

let handle_image_link url href label =
  match url with
  | Embed_data s ->
    [ Xml.block "img" ~attr:[ ("src", s); ("title", Inline.asciis label) ] [] ]
  | Complex { protocol; link } ->
    (* slight hack here to handle math2png annotations *)
    let opts, href =
      try
        Scanf.sscanf protocol "depth-%d" (fun n ->
            ([ ("style", Printf.sprintf "vertical-align: -%dpx" n) ], link))
      with _ -> ([], href)
    in
    [ Xml.block "img"
        ~attr:(opts @ [ ("src", href); ("title", Inline.asciis label) ])
        []
    ]
  | Search _
  | Page_ref _
  | Block_ref _
  | File _ ->
    [ Xml.block "img" ~attr:[ ("src", href); ("title", Inline.asciis label) ] []
    ]

let rec range t stopped =
  let open Range in
  let { start; stop } = t in
  Xml.block "div"
    ~attr:[ ("class", "timestamp-range"); ("stopped", string_of_bool stopped) ]
    [ timestamp start "Start"; timestamp stop "Stop" ]

and timestamp ({ active; _ } as t) kind =
  let prefix =
    match kind with
    | "Scheduled" ->
      Xml.raw "<i class=\"fa fa-calendar\" style=\"margin-right:6px;\"></i>"
    | "Deadline" ->
      Xml.raw
        "<i class=\"fa fa-calendar-times-o\" style=\"margin-right:6px;\"></i>"
    | "Date" -> Xml.empty
    | "Closed" -> Xml.empty
    | "Started" ->
      Xml.raw "<i class=\"fa fa-clock-o\" style=\"margin-right:6px;\"></i>"
    | "Start" -> Xml.data "From: "
    | "Stop" -> Xml.data "To: "
    | _ -> Xml.empty
  in
  Xml.block "span"
    ~attr:
      [ ( "class"
        , "timestamp "
          ^
          if kind = "Closed" then
            "line-through"
          else
            "" )
      ; ( "active"
        , if active then
            "true"
          else
            "false" )
      ]
    [ prefix; Xml.data (to_string t) ]

let rec map_inline config l = concatmap (inline config) l

and inline config t =
  let open List in
  match t with
  | Plain s
  | Spaces s ->
    [ Xml.data s ]
  | Superscript l -> [ Xml.block "sup" (map_inline config l) ]
  | Subscript l -> [ Xml.block "sub" (map_inline config l) ]
  | Emphasis (kind, data) ->
    let l =
      [ (`Bold, "b")
      ; (`Italic, "i")
      ; (`Underline, "ins")
      ; (`Strike_through, "del")
      ; (`Highlight, "mark")
      ]
    in
    [ Xml.block (assoc kind l) (map_inline config data) ]
  | Entity e -> [ Xml.raw e.html ]
  | Tag t ->
    [ Xml.block "a" ~attr:[ ("class", "tag") ]
        [ Xml.data (Inline.hash_tag_value_string (Tag t)) ]
    ]
  | Latex_Fragment (Displayed s) -> [ Xml.data ("\\[" ^ s ^ "\\]") ]
  | Latex_Fragment (Inline s) -> [ Xml.data ("\\(" ^ s ^ "\\)") ]
  | Target s -> [ Xml.block "a" ~attr:[ ("id", s) ] [ Xml.data s ] ]
  | Link { url; label; _ } -> (
      match url with
      | Page_ref s -> [ Xml.data s ]
      | _ ->
        let href = Inline.string_of_url url in
        (* If it is an image *)
        if
          Inline.is_embed_data url
          || List.exists (ends_with href)
            [ ".png"; ".jpg"; ".jpeg"; ".svg"; ".ico"; ".gif"; ".bmp" ]
        then
          handle_image_link url href label
        else
          let href =
            match url with
            | Search x -> "#" ^ Type_parser.Heading.anchor_link x
            | _ -> href
          in
          let label =
            match url with
            | Search s -> [ Xml.data s ]
            | _ -> map_inline config label
          in
          [ Xml.block "a" ~attr:[ ("href", href) ] label ])
  | Verbatim s
  | Code s ->
    [ Xml.block "code" [ Xml.data s ] ]
  | Inline_Source_Block x -> [ Xml.block "code" [ Xml.data x.code ] ]
  | Export_Snippet ("html", s) -> [ Xml.raw s ]
  | Break_Line
  | Hard_Break_Line ->
    [ Xml.block "br" [] ]
  | Timestamp (Scheduled t) -> [ timestamp t "Scheduled" ]
  | Timestamp (Deadline t) -> [ timestamp t "Deadline" ]
  | Timestamp (Date t) -> [ timestamp t "Date" ]
  | Timestamp (Range t) -> [ range t false ]
  | Timestamp (Closed t) -> [ timestamp t "Closed" ]
  | Timestamp (Clock (Stopped t)) -> [ range t true ]
  | Timestamp (Clock (Started t)) -> [ timestamp t "Started" ]
  | Cookie (Percent n) ->
    [ Xml.block "span"
        ~attr:[ ("class", "cookie-percent") ]
        [ Xml.data ("[" ^ string_of_int n ^ "%" ^ "]") ]
    ]
  | Cookie (Absolute (current, total)) ->
    [ Xml.block "span"
        ~attr:[ ("class", "cookie-absolute") ]
        [ Xml.data
            ("[" ^ string_of_int current ^ "/" ^ string_of_int total ^ "]")
        ]
    ]
  | Footnote_Reference { name; _ } ->
    let encode_name = Uri.pct_encode name in
    [ Xml.block "sup"
        [ Xml.block "a"
            ~attr:
              [ ("id", "fnr." ^ encode_name)
              ; ("class", "footref")
              ; ("href", "#fn." ^ encode_name)
              ]
            [ Xml.data name ]
        ]
    ]
  | Macro { name; arguments } -> (
      if name = "cloze" then
        [ Xml.Data (String.concat "," arguments) ]
      else
        try
          let value = List.assoc name !macros in
          let buff = Buffer.create (String.length value) in
          Buffer.add_substitute buff
            (fun v -> try List.nth arguments (int_of_string v - 1) with _ -> v)
            value;
          let content = Buffer.contents buff in
          match
            Angstrom.parse_string ~consume:All (Inline.parse config) content
          with
          | Ok inlines -> map_inline config (List.map fst inlines)
          | Error _e -> [ Xml.empty ]
        with Not_found -> [ Xml.empty ])
  | _ -> [ Xml.empty ]

let get_int_option name =
  try
    match List.assoc name !options with
    | "nil" -> 0
    | s when is_number s -> int_of_string s
    | _ -> 1024
  with Not_found -> 1024

let construct_numbering config ?(toc = false) level numbering =
  let num_option = get_int_option "num" in
  if config.heading_number then
    if level <= num_option then
      match numbering with
      | Some l ->
        let numbering = List.map string_of_int l |> String.concat "." in
        if toc then
          Xml.data (numbering ^ ". ")
        else
          Xml.block "span"
            ~attr:[ ("class", "numbering"); ("style", "margin-right:6px") ]
            [ Xml.data numbering ]
      | None -> Xml.empty
    else
      Xml.empty
  else
    Xml.empty

let size_to_hN size =
  let size' =
    if size > 6 || size < 1 then
      6
    else
      size
  in
  "h" ^ string_of_int size'

let concat_elements l r =
  match l with
  | [] -> [r]
  | _ ->
    if (last l) == Xml.empty then
      l @ [r]
    else
      l @ [(Xml.block "span" [Xml.data " "]); r]

let heading config { title; tags; marker; level; priority; numbering; size; _ } =
  let numbering = construct_numbering config level numbering in
  let marker =
    match marker with
    | Some v ->
      Xml.block "span"
        ~attr:
          [ ("class", "task-status " ^ String.lowercase_ascii v) ]
        [ Xml.data (String.uppercase_ascii v); ]
    | None -> Xml.empty
  in
  let priority =
    match priority with
    | Some v ->
      Xml.block "span"
        ~attr:[ ("class", "priority") ]
        [ Xml.data (Util.priority_to_string v) ]
    | None -> Xml.empty
  in
  let tags =
    match tags with
    | [] -> Xml.empty
    | tags ->
      Xml.block "span"
        ~attr:[ ("class", "heading-tags") ]
        (List.map
           (fun tag ->
              Xml.block "span" ~attr:[ ("class", "tag") ]
                [ Xml.block "span" ~attr:[ ("class", tag) ] [ Xml.data tag ] ])
           tags)
  in

  let elements = numbering :: marker :: priority
                 :: (Xml.block "span" (map_inline config (Type_op.inline_list_strip_pos title)))
                 :: [ tags ] in
  let result = List.fold_left (fun acc e -> concat_elements acc e) [] elements in
  let r = Xml.list result in
  match size with
  | Some s -> Xml.block (size_to_hN s) [ r ]
  | None -> r

let rec list_item config x =
  let content =
    match x.content with
    | [] -> [ Xml.empty ]
    | Paragraph i :: rest ->
      map_inline config (Type_op.inline_list_strip_pos i) @ blocks' config rest
    | _ -> blocks' config x.content
  in
  let checked, checked_html =
    match x.checkbox with
    | Some x ->
      ( x
      , if x then
          Xml.raw
            "<i class=\"fa fa-check-square-o\" style=\"margin-right:6px;\"></i>"
        else
          Xml.raw "<i class=\"fa fa-square-o\" style=\"margin-right:6px;\"></i>"
      )
    | _ -> (false, Xml.empty)
  in
  let items =
    if List.length x.items = 0 then
      Xml.empty
    else
      Xml.block (list_element x.items) (concatmap (list_item config) x.items)
  in
  match x.number with
  | None ->
    let block =
      match x.name with
      | [] ->
        Xml.block
          ~attr:[ ("checked", string_of_bool checked) ]
          "li"
          [ Xml.block "p" (checked_html :: content); items ]
      | inlines ->
        Xml.block
          ~attr:[ ("checked", string_of_bool checked) ]
          "dl"
          [ Xml.block "dt"
              (map_inline config (Type_op.inline_list_strip_pos inlines))
          ; Xml.block "dd" (content @ [ items ])
          ]
    in
    [ block ]
  | Some number ->
    [ Xml.block
        ~attr:
          [ ("style", "list-style-type: none")
          ; ("checked", string_of_bool checked)
          ]
        "li"
        [ Xml.block "p"
            (Xml.data (string_of_int number ^ ". ") :: checked_html :: content)
        ; items
        ]
    ]

and table config { header; groups; col_groups } =
  let tr elm cols =
    Xml.block "tr"
      (List.map
         (fun col ->
            Xml.block elm
              ~attr:[ ("scope", "col"); ("class", "org-left") ]
              (map_inline config col))
         cols)
  in
  let col_groups =
    try
      List.map
        (fun number ->
           let col_elem = Xml.block "col" ~attr:[ ("class", "org-left") ] [] in
           Xml.block "colgroup" (repeat number col_elem))
        col_groups
    with _ -> []
  in
  let head =
    match header with
    | None -> Xml.empty
    | Some cols -> Xml.block "thead" [ tr "th" cols ]
  in
  let groups =
    List.map (fun group -> Xml.block "tbody" (List.map (tr "td") group)) groups
  in
  Xml.block
    ~attr:
      [ ("border", "2")
      ; ("cellspacing", "0")
      ; ("cellpadding", "6")
      ; ("rules", "groups")
      ; ("frame", "hsides")
      ]
    "table"
    (col_groups @ (head :: groups))

and blocks' config l = List.map (block config) l

and block config t =
  let open List in
  match t with
  | Paragraph l ->
    Xml.block "p" (map_inline config (Type_op.inline_list_strip_pos l))
  | Horizontal_Rule -> Xml.block "hr" []
  | Heading h -> heading config h
  | List l -> Xml.block (list_element l) (concatmap (list_item config) l)
  | Table t -> table config t
  | Displayed_Math s ->
    Xml.block "div" ~attr:[ ("class", "mathblock") ]
      [ Xml.data ("$$" ^ s ^ "$$") ]
  | Example l -> Xml.block "pre" [ Xml.data (String.concat "" l) ]
  | Src { language; lines; _ } ->
    let attr =
      match language with
      | None -> []
      | Some l -> [ ("data-lang", l); ("class", l) ]
    in
    Xml.block "pre"
      [ Xml.block "code" ~attr [ Xml.data (String.concat "" lines) ] ]
  | Quote l -> Xml.block "blockquote" (blocks' config l)
  | Export ("html", _options, content) -> Xml.raw content
  | Raw_Html content -> Xml.raw content
  | Custom (name, _options, result, _content) ->
    Xml.block "div" ~attr:[ ("class", name) ] (blocks' config result)
  | Latex_Fragment l ->
    Xml.block "p"
      ~attr:[ ("class", "latex-fragment") ]
      (inline config (Inline.Latex_Fragment l))
  | Latex_Environment (name, option, content) ->
    let option =
      match option with
      | None -> ""
      | Some s -> s
    in
    let content =
      "\n\\begin{" ^ name ^ "} " ^ option ^ "\n" ^ content ^ "\n\\end{" ^ name
      ^ "}"
    in
    Xml.block "div"
      ~attr:[ ("class", "latex-environment") ]
      [ Xml.data content ]
  | Footnote_Definition (name, definition) ->
    let encode_name = Uri.pct_encode name in
    Xml.block "div" ~attr:[ ("class", "footdef") ]
      [ Xml.block "div" ~attr:[ ("class", "footpara") ]
          [ block config (Paragraph definition) ]
      ; Xml.block "sup"
          [ Xml.block "a"
              ~attr:
                [ ("id", "fn." ^ encode_name)
                ; ("class", "footnum")
                ; ("href", "#fnr." ^ encode_name)
                ]
              [ Xml.data (name ^ "↩︎") ]
          ]
      ]
  | _ -> Xml.empty

let blocks_aux config v =
  let open Zip in
  let rec aux v =
    match v with
    | Leaf (b, _) -> [ block config b ]
    | Branch [] -> []
    | Branch (Leaf (h, _) :: t) ->
      let heading = block config h in
      [ Xml.block "ul"
          [ Xml.block "li" (List.flatten ([ heading ] :: List.map aux t)) ]
      ]
    | Branch l -> List.flatten @@ List.map aux l
  in
  let v' = aux v in
  let rec merge_ul v =
    match v with
    | Xml.Block ("ul", _, l1) :: Xml.Block ("ul", _, l2) :: t ->
      merge_ul (Xml.block "ul" (l1 @ l2) :: t)
    | Xml.Block (tag, attrs, l) :: t ->
      Xml.Block (tag, attrs, merge_ul l) :: merge_ul t
    | h :: t -> h :: merge_ul t
    | [] -> []
  in
  merge_ul v'

let blocks config tl refs =
  let open Tree_type in
  let v = of_blocks tl |> replace_embed_and_refs ~refs |> to_value in
  blocks_aux config v

let toc config content =
  let toc_option = get_int_option "toc" in
  let rec go content =
    match hd_opt content with
    | None -> Xml.empty
    | Some { level; _ } ->
      if level > toc_option then
        Xml.empty
      else
        let items =
          List.map
            (fun { title; level; anchor; numbering; items } ->
               let numbering =
                 construct_numbering config ~toc:true level (Some numbering)
               in
               let link =
                 Xml.block "a"
                   ~attr:[ ("href", "#" ^ anchor) ]
                   (numbering
                    :: map_inline config (Type_op.inline_list_strip_pos title))
               in
               Xml.block "li" [ link; go items ])
            content
        in
        Xml.block "ul" items
  in
  if toc_option > 0 then
    let items = go content in
    Xml.block "div" ~attr:[ ("id", "toc") ]
      [ Xml.block "h2" [ Xml.data "Table of contents" ]; items ]
  else
    Xml.empty

let collect_macros directives =
  let collected =
    directives
    |> List.filter (fun (name, _) -> String.uppercase_ascii name = "MACRO")
    |> List.map (fun (_, df) ->
        let name, definition = splitl (fun c -> c <> ' ') df in
        (name, String.trim definition))
  in
  macros := collected

let collect_options directives =
  let collected =
    try
      let options =
        List.find
          (fun (name, _) -> String.uppercase_ascii name = "OPTIONS")
          directives
      in
      snd options |> String.split_on_char ' '
      |> List.map (fun s ->
          match String.split_on_char ':' s with
          | [] -> ("", "")
          | [ k; v ] -> (k, v)
          | a -> (List.hd a, List.nth a 1))
    with Not_found -> []
  in
  options := collected

let directives_to_string directives =
  Document.directives_to_yojson directives |> Yojson.Safe.to_string

module HtmlExporter = struct
  let name = "html"

  let default_filename = change_ext "html"

  let export ~refs config doc output =
    let refs =
      Option.default
        Reference.{ parsed_embed_blocks = []; parsed_embed_pages = [] }
        refs
    in
    (* let { filename; blocks; directives; title; author; toc } = doc in *)
    collect_macros doc.directives;
    collect_options doc.directives;

    let blocks = blocks config doc.blocks refs in
    let blocks =
      if Conf.(config.toc) then
        toc config doc.toc :: blocks
      else
        blocks
    in
    let body =
      [ Xml.raw
          ("<!-- directives: " ^ directives_to_string doc.directives ^ " -->\n")
      ; Xml.block "div" ~attr:[ ("id", "content") ] blocks
      ]
    in
    Xml.output_xhtml output body
end

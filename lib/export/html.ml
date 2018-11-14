open Prelude
open Type
open Inline
open Document
open Timestamp

(* taken from mlorg *)

let macros = ref []

(* #+OPTIONS:   num:nil toc:2 *)
let options = ref []

let concatmap f l = List.concat (List.map f l)

let list_element = function
  | [] -> "ul"
  | { ordered; name } :: tl ->
    let name = match name with | None -> false | Some _ -> true in
    if name then "dl"
    else if ordered then "ol"
    else "ul"

let handle_image_link url href label =
  match url with
  | Complex {protocol; link} ->
    (* slight hack here to handle math2png annotations *)
    let opts, href =
      try
        Scanf.sscanf protocol "depth-%d" (fun n ->
            ( [("style", Printf.sprintf "vertical-align: -%dpx" n)]
            , link ) )
      with _ -> ([], href)
    in
    [ Xml.block "img"
        ~attr:(opts @ [("src", href); ("title", Inline.asciis label)])
        [] ]
  | Search s | File s ->
    [ Xml.block "img"
        ~attr:[("src", href); ("title", Inline.asciis label)]
        [] ]

let rec range {start; stop} stopped =
  Xml.block "div"
    ~attr:
      [("class", "timestamp-range"); ("stopped", string_of_bool stopped)]
    [timestamp start "Start"; timestamp stop "Stop"]

and timestamp ({active; date; time; repetition} as t) kind =
  let prefix =
    match kind with
    | "Scheduled" ->
      Xml.raw
        "<i class=\"fa fa-calendar\" style=\"margin-right:6px;\"></i>"
    | "Deadline" ->
      Xml.raw
        "<i class=\"fa fa-calendar-times-o\" \
         style=\"margin-right:6px;\"></i>"
    | "Date" -> Xml.empty
    | "Closed" -> Xml.empty
    | "Started" ->
      Xml.raw
        "<i class=\"fa fa-clock-o\" style=\"margin-right:6px;\"></i>"
    | "Start" -> Xml.data "From: "
    | "Stop" -> Xml.data "To: "
    | _ -> Xml.empty
  in
  Xml.block "span"
    ~attr:
      [ ( "class"
        , "timestamp " ^ if kind = "Closed" then "line-through" else ""
        )
      ; ("active", if active then "true" else "false") ]
    [prefix; Xml.data (to_string t)]

let rec map_inline l = concatmap inline l

and inline t =
  let open List in
  match t with
  | Plain s -> [Xml.data s]
  | Superscript l -> [Xml.block "sup" (map_inline l)]
  | Subscript l -> [Xml.block "sub" (map_inline l)]
  | Emphasis (kind, data) ->
    let l = [`Bold, "b"; `Italic, "i"; `Underline, "u"; `Strike_through, "del"] in
    [Xml.block (assoc kind l) (map_inline data)]
  | Entity e ->
    [Xml.raw e.html]
  | Latex_Fragment (Displayed s) ->
    [Xml.data ("\\["^s^"\\]")]
  | Latex_Fragment (Inline s) ->
    [Xml.data ("\\("^s^"\\)")]
  | Target s -> [Xml.block "a" ~attr:[("id", s)] []]
  | Link {url; label} ->
    let href = Inline.string_of_url url in
    (* If it is an image *)
    if List.exists (ends_with href)
        [".png"; ".jpg"; ".jpeg"; ".gif"; ".bmp"] then
      handle_image_link url href label
    else
      let href = match url with
        | Search x -> "#" ^ Heading.anchor_link x
        | _ -> href
      in
      [Xml.block "a" ~attr: ["href", href]
         (map_inline label)]
  | Verbatim s | Code s ->
    [Xml.block "code" [Xml.data s]]
  | Inline_Source_Block x ->
    [Xml.block "code" [Xml.data x.code]]
  | Export_Snippet ("html", s) ->
    [Xml.raw s]
  | Break_Line ->
    [Xml.block "br" []]
  | Timestamp (Scheduled t) -> [timestamp t "Scheduled"]
  | Timestamp (Deadline t) -> [timestamp t "Deadline"]
  | Timestamp (Date t) -> [timestamp t "Date"]
  | Timestamp (Range t) -> [range t false]
  | Timestamp (Closed t) -> [timestamp t "Closed"]
  | Timestamp (Clock (Stopped t)) -> [range t true]
  | Timestamp (Clock (Started t)) -> [timestamp t "Started"]
  | Cookie (Percent n) ->
    [ Xml.block "span"
        ~attr:[("class", "cookie-percent")]
        [Xml.data ("[" ^ string_of_int n ^ "%" ^ "]")]]
  | Cookie (Absolute (current, total)) ->
    [ Xml.block "span"
        ~attr:[("class", "cookie-absolute")]
        [Xml.data ("[" ^ (string_of_int current) ^ "/" ^ (string_of_int total) ^ "]")]]
  | Footnote_Reference { id; name } ->
    [ Xml.block "sup"
        [ Xml.block "a"
            ~attr:[("id", "fnr." ^ (string_of_int id));
                   ("class", "footref");
                   ("href", "#fn." ^ name)]
            [Xml.data name]]]
  | Macro {name; arguments} ->
    (try
       let value = (List.assoc name !macros) in
       let buff = Buffer.create (String.length value) in
       Buffer.add_substitute buff
         (fun v -> try List.nth arguments (int_of_string v - 1) with _ -> v)
         value ;
       let content = (Buffer.contents buff) in
       match Angstrom.parse_string Inline.parse content with
       | Ok inlines -> map_inline inlines
       | Error e -> [Xml.empty]
     with Not_found ->
       [Xml.empty])
  | _ ->
    [Xml.empty]

let get_int_option name =
  try
    match List.assoc name !options with
    | "nil" -> 0
    | s when is_number s -> int_of_string s
    | _ -> 1024
  with Not_found -> 1024

let construct_numbering ?(toc=false) level numbering =
  let num_option = get_int_option "num" in
  if level <= num_option then
    match numbering with
    | Some l ->
      let numbering =
        List.map string_of_int l |> String.concat "." in
      if toc then
        Xml.data (numbering ^ ". ")
      else
        Xml.block "span"
          ~attr:[("class", "numbering");
                 ("style", "margin-right:6px")]
          [Xml.data numbering]
    | None -> Xml.empty
  else
    Xml.empty

let heading {title; tags; marker; level; priority; anchor; meta; numbering} =
  let numbering = construct_numbering level numbering in
  let marker =
    match marker with
    | Some v ->
      Xml.block "span"
        ~attr:[("class", "task-status " ^ String.lowercase_ascii v);
               ("style", "margin-right:6px")]
        [Xml.data (String.uppercase_ascii v)]
    | None -> Xml.empty
  in
  let priority =
    match priority with
    | Some v ->
      Xml.block "span"
        ~attr:[("class", "priority");
               ("style", "margin-right:6px")]
        [Xml.data ("[#" ^ String.make 1 v ^ "]")]
    | None -> Xml.empty
  in
  let tags =
    match tags with
    | [] -> Xml.empty
    | tags ->
      Xml.block "span"
        ~attr:[("class", "heading-tags")]
        (List.map
           (fun tag ->
              Xml.block "span"
                ~attr:[("class", "headding-tag"); ("style", "padding-left:6px")]
                [Xml.data tag])
           tags)
  in
  Xml.block (Printf.sprintf "h%d" level)
    ~attr:["id", anchor]
    (numbering :: marker :: priority :: map_inline title @ [tags])

let rec list_item x =
  let content =
    match x.content with
    | Paragraph i :: rest -> map_inline i @ blocks rest
    | _ -> blocks x.content
  in
  let checked, checked_html =
    match x.checkbox with
    | Some x ->
      ( x
      , if x then
          Xml.raw
            "<i class=\"fa fa-check-square-o\" \
             style=\"margin-right:6px;\"></i>"
        else
          Xml.raw
            "<i class=\"fa fa-square-o\" \
             style=\"margin-right:6px;\"></i>" )
    | _ -> (false, Xml.empty)
  in
  let items = if List.length x.items = 0 then Xml.empty
    else Xml.block (list_element x.items) (concatmap list_item x.items) in
  match x.number with
  | None ->
    let block = match x.name with
      | None -> (Xml.block
                   ~attr:[("checked", string_of_bool checked)]
                   "li"
                   [Xml.block "p" (checked_html :: content);
                    items])
      | Some name ->
        (Xml.block
           ~attr:[("checked", string_of_bool checked)]
           "dl"
           [Xml.block "dt" [(Xml.data name)];
            Xml.block "dd" (content @ [items])])
    in
    [ block ]
  | Some number ->
    [ Xml.block
        ~attr:
          [ ("style", "list-style-type: none")
          ; ("checked", string_of_bool checked) ]
        "li"
        [ Xml.block "p"
            (Xml.data (string_of_int number ^ ". ")
             :: checked_html
             :: content);
          items] ]

and table { header; groups} =
  let tr cols =
    Xml.block "tr"
      (List.map (fun col ->
           (Xml.block "th" ~attr:[("scope", "col");
                                  ("class", "org-right")]
              (map_inline col)))
          cols) in
  let head = match header with
    | None -> Xml.empty
    | Some cols ->
      Xml.block "thead" [tr cols] in
  let groups = List.map (fun group ->
      Xml.block "tbody" (List.map tr group)
    ) groups in
  Xml.block ~attr:[("border", "1")] "table"
    ([head] @ groups)

and blocks l = List.map block l
and block t =
  let open List in
  match t with
  | Paragraph l -> Xml.block "p" (map_inline l)
  | Horizontal_Rule -> Xml.block "hr" []
  | Heading h ->
    heading h
  | List l -> Xml.block (list_element l) (concatmap list_item l)
  | Table t -> table t
  | Math s ->
    Xml.block "div" ~attr:["class", "mathblock"]
      [Xml.data ("$$" ^ s ^ "$$")]
  | Example l -> Xml.block "pre" [Xml.data (String.concat "\n" l)]
  | Src {language; options; lines} -> Xml.block "pre" [Xml.data (String.concat "\n" lines)]
  | Quote l ->
    Xml.block "blockquote" (blocks l)
  | Export ("html", options, content) ->
    Xml.raw content
  | Custom (name, options, l) ->
    Xml.block "div" ~attr:["class", name]
      (blocks l)
  | Latex_Fragment l ->
    Xml.block "p" ~attr:["class", "latex-fragment"]
      (inline (Inline.Latex_Fragment l))
  | Latex_Environment (name, option, content) ->
    let option = match option with | None -> "" | Some s -> s in
    let content = "\n\\begin{" ^ name ^ "} " ^ option ^ "\n"
                  ^ (String.concat "\n" content)
                  ^ "\n\\end{" ^ name ^ "}" in
    Xml.block "div" ~attr:["class", "latex-environment"]
      [Xml.data content]
  | Footnote_Definition (name, definition) ->
    Xml.block "div" ~attr:["class", "footdef"]
      [(Xml.block "sup"
          [(Xml.block "a"
              ~attr:[("id", "fn." ^ name);
                     ("class", "footnum");
                     ("href", "#fnr.1")]
              [Xml.data name])]);
       (Xml.block "div" ~attr:["class", "footpara"]
          [block (Paragraph definition)])]
  | _ -> Xml.empty

let toc content =
  let toc_option = get_int_option "toc" in
  let rec go content =
    match Prelude.hd_opt content with
    | None -> Xml.empty
    | Some { level } ->
      if level > toc_option then
        Xml.empty
      else
        let items = (List.map (fun { title; level; anchor; numbering; items } ->
            let numbering = construct_numbering ~toc:true level (Some numbering) in
            let link = Xml.block "a" ~attr: ["href", "#" ^ anchor]
                (numbering :: map_inline title) in
            Xml.block "li"
              (link :: [go items])
          ) content) in
        Xml.block "ul" items
  in
  if toc_option > 0 then
    let items = go content in
    Xml.block "div"
      ~attr:[("id", "toc")]
      ((Xml.block "h2" [Xml.data "Table of contents"]) :: [items])
  else
    Xml.empty

let collect_macros directives =
  let collected = directives
                  |> List.filter (fun (name, _) -> (String.uppercase_ascii name) = "MACRO")
                  |> List.map (fun (_, df) ->
                      let (name, definition) = splitl (fun c -> c <> ' ') df in
                      (name, String.trim definition)) in
  macros := collected

let collect_options directives =
  let collected = try
      let options = List.find (fun (name, _) -> (String.uppercase_ascii name) = "OPTIONS") directives in
      snd options |> String.split_on_char ' '
      |> List.map (fun s ->
          match String.split_on_char ':' s with
          | [] -> ("", "")
          | [k; v] -> (k, v)
          | a -> (List.hd a, List.nth a 1))
    with Not_found -> [] in
  options := collected

module HtmlExporter = struct
  let name = "html"

  let default_filename = change_ext "html"

  let export doc output =
    (* let { filename; blocks; directives; title; author; toc } = doc in *)
    collect_macros doc.directives;
    collect_options doc.directives;
    let subtitle = match doc.subtitle with
      | None -> Xml.empty
      | Some s -> Xml.block "span" ~attr:["class", "subtitle"]
                    [(Xml.data s)] in
    let title = match doc.title with
      | None -> Xml.empty
      | Some s -> Xml.block "h1" ~attr:["class", "title"]
                    [Xml.data s; Xml.raw "<br />"; subtitle] in

    let toc = toc doc.toc in
    let body = [Xml.block "div" ~attr:["id", "content"]
                  (title :: toc :: (List.map block doc.blocks))
               ] in
    Xml.output_xhtml output body
end

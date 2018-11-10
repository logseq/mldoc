open Prelude
open Type
open Inline
open Document

let concatmap f l = List.concat (List.map f l)

let list_element = function
  | [] -> "ul"
  | { ordered } :: tl -> if ordered then "ol" else "ul"

let rec map_inline l = concatmap inline l

(* TODO: footnote *)
and inline t =
  let open List in
  match t with
  | Plain s -> [Xml.data s]
  | Superscript l -> [Xml.block "sup" (map_inline l)]
  | Subscript l -> [Xml.block "sub" (map_inline l)]
  | Emphasis (kind, data) ->
    let l = [`Bold, "b"; `Italic, "i"; `Underline, "u"] in
    [Xml.block (assoc kind l) (map_inline data)]
  | Entity e ->
    [Xml.raw e.html]
  | Latex_Fragment (Inline s) ->
    [Xml.data ("\\("^s^"\\)")]
  (* | Link {url; label} ->
   *   let href = Inline.string_of_url url in
   *   (\* If it is an image *\)
   *   if List.exists (String.ends_with href)
   *       (Config.get config image_extensions) then
   *     self#handle_image_link url href label
   *   else
   *     let href = match url with
   *       | Search x -> "#" ^ Toc.link x
   *       | _ -> href
   *     in
   *     [Xml.block "a" ~attr: ["href", href]
   *        (self#inlines label)] *)
  | Verbatim s ->
    [Xml.block "code" [Xml.data s]]
  (* | Inline_Source_Block x ->
   *   Xml.block "code" [Xml.data x.code] *)
  (* | Export_Snippet ("html", s) ->
   *   Xml.raw s *)
  | Break_Line ->
    [Xml.block "br" []]
  | Target s ->
    [Xml.block "a" ~attr:["id", s] []]
  | _ ->
    [Xml.empty]

let heading {title; tags; marker; level; priority; anchor; meta} =
  let marker =
    match marker with
    | Some v -> (
        match v with
        | "TODO" | "todo" ->
          Xml.raw "<span class=\"task-status todo\">TODO</span>"
        | "DONE" | "done" ->
          Xml.raw "<span class=\"task-status done\">DONE</span>"
        | v ->
          Xml.raw
            (Printf.sprintf "<span class=\"task-status %s\">%s</span>"
               (String.lowercase_ascii v) (String.uppercase_ascii v)) )
    | None -> Xml.empty
  in
  let priority =
    match priority with
    | Some v ->
      Xml.raw (Printf.sprintf "<span class=\"priority\">%c</span>" v)
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
              Xml.raw
                (Printf.sprintf "<span class=\"heading-tag\">%s</span>"
                   tag) )
           tags)
  in
  Xml.block (Printf.sprintf "h%d" level)
    ~attr:["id", anchor]
    (marker :: priority :: map_inline title @ [tags])

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
    [ Xml.block
        ~attr:[("checked", string_of_bool checked)]
        "li"
        [Xml.block "p" (checked_html :: content);
         items] ]
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
  | Custom (name, options, l) ->
    Xml.block "div" ~attr:["class", name]
      (blocks l)
  | _ -> Xml.empty

module HtmlExporter = struct
  let name = "html"

  let default_filename = change_ext "html"

  let export doc output =
    (* let { filename; blocks; directives; title; author; toc } = doc in *)
    let title = match doc.title with
      | None -> Xml.empty
      | Some s -> Xml.block "h1" ~attr:["class", "title"]
                    [(Xml.data s)] in
    let body = [Xml.block "div" ~attr:["id", "content"]
                  (title :: (List.map block doc.blocks))
               ] in
    Xml.output_xhtml output body
end

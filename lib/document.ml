open Type

type toc = toc_item list [@@deriving yojson]

and toc_item =
  { title : Inline.t list
  ; level : int
  ; anchor : string
  ; numbering : int list
  ; items : toc
  }

type directives = (string * string) list [@@deriving yojson]

(**
    A document is:
   - some content before the first heading
   - a list of top-level headings
   - a list of directive
   - the footnotes inside the beginning of the file.
*)
type t =
  { filename : string option  (** The filename the document was parsed from *)
  ; blocks : blocks  (** Blocks content *)
  ; directives : directives  (** The directives present in the file *)
  ; title : string option [@default None]  (** The document's title *)
  ; subtitle : string option [@default None]  (** The document's subtitle *)
  ; author : string option [@default None]  (** The document's author *)
  ; toc : toc  (** Table of content *)
  }
[@@deriving yojson]

exception Non_timestamp

let get_timestamps inlines =
  let open Inline in
  try
    List.fold_left
      (fun acc t ->
        match t with
        | Timestamp t -> t :: acc
        | Break_Line
        | Hard_Break_Line ->
          acc
        | _ -> raise Non_timestamp)
      [] inlines
  with Non_timestamp -> []

let compute_heading_numbering level toc =
  match toc with
  | [] -> [ 1 ]
  | p :: _ ->
    let open List in
    let open Prelude in
    if p.level = level then
      drop_last 1 p.numbering @ [ last p.numbering + 1 ]
    else if p.level < level then
      (* child *)
      p.numbering @ [ 1 ]
    else
      (* breakout *)
      let diff = p.level - level in
      let offset = List.length p.numbering - (diff + 1) in
      let before, after = Prelude.split_n offset p.numbering in
      before @ [ hd after + 1 ]

let ast_to_json ast = toc_item_to_yojson ast |> Yojson.Safe.to_string

let rec toc_append_item parent depth item =
  if depth = 1 then
    { parent with items = parent.items @ [ item ] }
  else
    let item =
      let parent' = Prelude.last parent.items in
      toc_append_item parent' (depth - 1) item
    in
    { parent with items = Prelude.drop_last 1 parent.items @ [ item ] }

let toc_tree items =
  let rec go acc = function
    | [] -> List.rev acc
    | ({ numbering; _ } as h) :: tl -> (
      match List.length numbering with
      | 1 ->
        (* parent *)
        go (h :: acc) tl
      | _ ->
        (* child *)
        let parent = List.hd acc in
        let depth = List.length numbering - 1 in
        let parent = toc_append_item parent depth h in
        go (parent :: List.tl acc) tl)
  in
  go [] items

let from_ast filename ast =
  let find_directive directives k =
    try
      let r = List.assoc k directives in
      Some r
    with Not_found -> None
  in
  let rec aut directives blocks toc = function
    | [] -> (List.rev directives, List.rev blocks, List.rev toc)
    | (h, pos_meta) :: tl -> (
      let update_meta f =
        match blocks with
        | (Heading heading, pos_meta) :: _tl ->
          (Heading (f heading), pos_meta) :: List.tl blocks
        | _ -> (h, pos_meta) :: blocks
      in
      match h with
      | Directive (k, v) ->
        let directives = (k, v) :: directives in
        aut directives blocks toc tl
      | Heading { title; tags; marker; level; priority; anchor; meta; unordered; _ } ->
        let numbering = compute_heading_numbering level toc in
        let h =
          Heading
            { title
            ; tags
            ; marker
            ; level
            ; priority
            ; anchor
            ; meta
            ; numbering = Some numbering
            ; unordered
            }
        in
        let toc_item = { title; level; anchor; numbering; items = [] } in
        aut directives ((h, pos_meta) :: blocks) (toc_item :: toc) tl
      | Paragraph inlines ->
        let blocks =
          match get_timestamps inlines with
          | [] -> (h, pos_meta) :: blocks
          | timestamps ->
            update_meta (fun heading ->
                let timestamps' =
                  List.append timestamps heading.meta.timestamps
                in
                { heading with
                  meta = { heading.meta with timestamps = timestamps' }
                })
        in
        aut directives blocks toc tl
      | Property_Drawer properties ->
        let blocks =
          update_meta (fun heading ->
              { heading with meta = { heading.meta with properties } })
        in
        aut directives blocks toc tl
      | _ -> aut directives ((h, pos_meta) :: blocks) toc tl)
  in
  let directives, blocks, toc = aut [] [] [] ast in
  { filename
  ; directives
  ; blocks
  ; title = find_directive directives "TITLE"
  ; subtitle = find_directive directives "SUBTITLE"
  ; author = find_directive directives "AUTHOR"
  ; toc = toc_tree toc
  }

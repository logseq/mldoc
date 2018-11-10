open Type

type toc = toc_item list [@@deriving yojson]
and toc_item =
  { title: Inline.t list
  ; level: int
  ; anchor: string }

(**
    A document is:
    - some content before the first heading
    - a list of top-level headings
    - a list of directive
    - the footnotes inside the beginning of the file.
*)
type t =
  { filename: string option (** The filename the document was parsed from *)
  ; blocks: blocks  (** Blocks content *)
  ; directives: (string * string) list
  (** The directives present in the file *)
  ; title: string option  (** The document's title *)
  ; author: string option  (** The document's author *)
  ; toc: toc               (** Table of content *)
  } [@@deriving yojson]

exception Non_timestamp
let get_timestamps inlines =
  let open Inline in
  try
    List.fold_left
      (fun acc t ->
         match t with
         | Timestamp t -> t :: acc
         | Break_Line -> acc
         | _ -> raise Non_timestamp
      )
      []
      inlines
  with Non_timestamp -> []

(* let build_doc filename ?config:conf ast = () *)
let build_doc filename ast =
  let find_directive directives k =
    try
      let r = List.assoc k directives in
      Some r
    with Not_found -> None in
  let rec aut directives blocks toc = function
    | [] -> (List.rev directives, List.rev blocks, List.rev toc)
    | h :: tl ->
      let update_meta f =
        match blocks with
        | [] -> h :: blocks
        | Heading heading :: tl -> Heading (f heading) :: List.tl blocks
        | _ -> h :: blocks in
      match h with
      | Directive (k, v) ->
        let directives = (k, v) :: directives in
        aut directives blocks toc tl
      | Heading {title; level; anchor} ->
        aut directives (h :: blocks) ({title; level; anchor} :: toc) tl
      | Paragraph inlines ->
        let blocks = (match get_timestamps inlines with
            | [] -> (h :: blocks)
            | timestamps ->
              update_meta (fun heading ->
                  let timestamps' = List.append timestamps heading.meta.timestamps in
                  {heading with meta =
                                  {heading.meta with
                                   timestamps = timestamps'}})

          ) in
        aut directives blocks toc tl
      | Property_Drawer properties ->
        let blocks = update_meta (fun heading ->
            {heading with meta =
                            {heading.meta with
                             properties = properties}}) in
        aut directives blocks toc tl
      | _ ->
        aut directives (h :: blocks) toc tl
  in
  let (directives, blocks, toc) = aut [] [] [] ast in
  { filename
  ; directives
  ; blocks
  ; title = find_directive directives "TITLE"
  ; author = find_directive directives "AUTHOR"
  ; toc
  }

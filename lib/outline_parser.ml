open Angstrom
open Parsers
open Prelude

type heading =
  { level : int
  ; marker : string option
  ; priority : char option
  ; unordered : bool
  ; size : int option
  }
[@@deriving yojson]

type t =
  | Heading of heading
  | Content of string
[@@deriving yojson]

type t_list = t list [@@deriving yojson]

let content_followed_by_heading_postions _config =
  both pos
    (unsafe_lookahead
       (choice
          [ Type_parser.Block.fenced_code_block *> pos
          ; Paragraph.parse *> pos
          ; Paragraph.sep *> pos
          ]))

let content_followed_by_heading config =
  content_followed_by_heading_postions config >>= fun (start, end_) ->
  Angstrom.take (end_ - start) >>| fun s -> Content s

let heading config =
  let h =
    lift3
      (fun (level, unordered, size) marker priority ->
        Heading { level; marker; priority; size; unordered })
      (Type_parser.Heading.level config)
      (optional (ws *> Type_parser.Heading.marker))
      (optional (ws *> Type_parser.Heading.priority))
  in
  choice [ h <* ws; h <* (end_of_line <|> end_of_input) ]

let merge_consecutive_content tl =
  let r, last =
    List.fold_left
      (fun (r, last_content) e ->
        match (last_content, e) with
        | None, Content s -> (r, Some [ s ])
        | Some ss, Content s -> (r, Some (s :: ss))
        | None, Heading _ -> (e :: r, None)
        | Some ss, Heading _ ->
          (e :: Content (String.concat "" (List.rev ss)) :: r, None))
      ([], None) tl
  in
  let r =
    match last with
    | None -> r
    | Some ss -> Content (String.concat "" (List.rev ss)) :: r
  in
  List.rev r

(* findout all links and metadata in content
   1. [[link]]
   2. ((block-id))
   3. #tag
   4. property
      - key:: value (md)
      - org-mode property syntax
*)

type reference =
  | Block_ref of string
  | Page_refs of string list
[@@deriving yojson]

type property_value =
  | S of string
  | Ref of reference
[@@deriving yojson]

type property_values = property_value list [@@deriving yojson]

type property = (string * property_values) list [@@deriving yojson]

let page_ref_p () =
  let state = Stack.create () in
  let p =
    fix (fun p ->
        let nested =
          list
            [ string "[["
            ; option "" page_name
            ; p
            ; option "" page_name
            ; string "]]"
            ]
          >>| fun s ->
          let new_page_name = String.concat "" s in
          Stack.push new_page_name state;
          new_page_name
        in
        let plain =
          list [ string "[["; page_name; string "]]" ] >>| fun s ->
          let new_page_name = String.concat "" s in
          Stack.push new_page_name state;
          new_page_name
        in
        choice [ nested; plain ])
  in
  p >>| fun s ->
  ( s
  , Stack.to_seq state |> List.of_seq
    |> List.map (fun s -> String.sub s 2 (String.length s - 4)) )

let property_and_link _config =
  peek_char >>= fun c ->
  match c with
  | None -> fail "meta_and_links_parser"
  | Some '(' -> block_ref_ignore_bracket >>| fun s -> Block_ref s
  | Some '[' -> page_ref_p () >>| fun (_, l) -> Page_refs l
  | Some _ -> failwith "xxx"

let md_outline_parser config =
  choice [ heading config; content_followed_by_heading config ]
  |> many >>| merge_consecutive_content

let org_outline_parser _config = failwith ""

let outline_parser (config : Conf.t) =
  match config.format with
  | Conf.Org -> org_outline_parser config
  | Conf.Markdown -> md_outline_parser config

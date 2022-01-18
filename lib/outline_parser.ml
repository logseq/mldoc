open Angstrom
open Parsers
open Prelude

type reference =
  | Block_ref of string
  | Page_refs of string list
[@@deriving yojson]

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
  | Properties of (string * string) list
  | Content_raw of string (* internal use *)
  | Content of (string * reference list)
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
  Angstrom.take (end_ - start) >>| fun s -> Content_raw s

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

let properties config = Drawer.parse_to_kvs config >>| fun kvs -> Properties kvs

let heading_and_properties config =
  (fun h c p ->
    match (c, p) with
    | None, None -> [ h ]
    | None, Some p -> [ h; p ]
    | Some c, None -> [ h; c ]
    | Some c, Some p -> [ h; c; p ])
  <$> heading config
  <*> optional (content_followed_by_heading config)
  <*> optional (take_while1 is_eol *> properties config)

let merge_consecutive_content tl =
  let r, last =
    List.fold_left
      (fun (r, last_content) e ->
        match (last_content, e) with
        | None, Content_raw s -> (r, Some [ s ])
        | Some ss, Content_raw s -> (r, Some (s :: ss))
        | None, Heading _
        | None, Properties _ ->
          (e :: r, None)
        | Some ss, Heading _
        | Some ss, Properties _ ->
          (e :: Content_raw (String.concat "" (List.rev ss)) :: r, None)
        | _, Content _ -> failwith "unreachable: merge_consecutive_content")
      ([], None) tl
  in
  let r =
    match last with
    | None -> r
    | Some ss -> Content_raw (String.concat "" (List.rev ss)) :: r
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

let reference_p =
  let link_delimters = [ '['; '('; '#' ] in
  let fallback =
    Angstrom.take 1 >>= fun _ ->
    skip_while (fun c -> not @@ List.mem c link_delimters) >>| fun _ -> None
  in
  peek_char >>= fun c ->
  match c with
  | None -> fail "meta_and_links_parser"
  | Some '(' ->
    block_ref_ignore_bracket >>| (fun s -> Some (Block_ref s)) <|> fallback
  | Some '[' ->
    page_ref_p () >>| (fun (_, l) -> Some (Page_refs l)) <|> fallback
  | Some '#' ->
    Angstrom.take 1
    >>= (fun _ ->
          Hash_tag.hashtag_name >>| fun s ->
          print_string s;
          match parse_string ~consume:All (page_ref_p ()) ("[[" ^ s ^ "]]") with
          | Ok (_, l) -> Some (Page_refs l)
          | Error _ -> failwith "unreachable: reference_p")
    <|> fallback
  | Some _ -> fallback

let extract_refs content =
  print_string content;
  match parse_string ~consume:All (many reference_p) content with
  | Ok refs -> List.filter_map identity refs
  | Error _ -> failwith "unreachable: extract_refs"

let md_outline_parser config =
  choice
    [ heading_and_properties config
    ; (content_followed_by_heading config >>| fun s -> [ s ])
    ]
  |> many >>| List.flatten >>| merge_consecutive_content
  >>| List.map (fun c ->
          match c with
          | Content_raw s -> Content (s, extract_refs s)
          | _ -> c)

let org_outline_parser _config = failwith ""

let outline_parser (config : Conf.t) =
  match config.format with
  | Conf.Org -> org_outline_parser config
  | Conf.Markdown -> md_outline_parser config

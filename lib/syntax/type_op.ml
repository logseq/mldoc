open! Prelude
open Pos
open Type

let remove_properties =
  List.filter (function
    | Type.Property_Drawer _ -> false
    | _ -> true)

let inline_list_with_dummy_pos (l : 'a list) : ('a * pos_meta option) list =
  List.map (fun i -> (i, Some Pos.dummy_pos)) l

let inline_list_with_none_pos (l : 'a list) : ('a * pos_meta option) list =
  List.map (fun i -> (i, None)) l

let inline_list_strip_pos (l : ('a * pos_meta option) list) : 'a list =
  List.map fst l

let inline_with_pos i start_pos end_pos =
  (i, Some ({ start_pos; end_pos } : Pos.pos_meta))

let inline_move_forward (i, pos) ~forward_pos =
  ( i
  , match pos with
    | None -> None
    | Some { start_pos; end_pos } ->
      Some
        { start_pos = start_pos + forward_pos; end_pos = end_pos + forward_pos }
  )

let inline_list_move_forward l forward_pos =
  List.map (inline_move_forward ~forward_pos) l

let rec type_move_forawrd t forward_pos =
  match t with
  | Paragraph l -> Paragraph (inline_list_move_forward l forward_pos)
  | Heading h ->
    Heading { h with title = inline_list_move_forward h.title forward_pos }
  | List items ->
    List
      (List.map
         (fun l ->
           { l with
             content =
               List.map (fun t -> type_move_forawrd t forward_pos) l.content
           ; name = inline_list_move_forward l.name forward_pos
           })
         items)
  | Quote l -> Quote (List.map (fun t -> type_move_forawrd t forward_pos) l)
  | Footnote_Definition (s, l) ->
    Footnote_Definition (s, inline_list_move_forward l forward_pos)
  | _ -> t

let unescaped_md_string s =
  let open Bytes in
  let b = of_string s in
  let n =
    if length b = 0 then
      ref 0
    else
      ref 1
  in
  for i = 0 to length b - 2 do
    n :=
      !n
      +
      match get b i with
      | '\\' when Parsers.is_md_escape_char (get b (i + 1)) -> 0
      | _ -> 1
  done;
  if !n = length b then
    s
  else
    let b' = create !n in
    n := 0;
    let i = ref 0 in
    let len_1 = length b - 1 in
    while !i <= len_1 do
      (match get b !i with
      | '\\' when !i < len_1 ->
        let c = get b (!i + 1) in
        if Parsers.is_md_escape_char c then
          set b' !n c
        else (
          set b' !n '\\';
          incr n;
          set b' !n c
        );
        incr i
      | c -> set b' !n c);
      incr n;
      incr i
    done;
    to_string b'

let map_plain t f =
  let rec inline_aux (t : Inline.t) =
    match t with
    | Inline.Emphasis (em_type, tl) ->
      Inline.Emphasis (em_type, List.map inline_aux tl)
    | Inline.Tag tl -> Inline.Tag (List.map inline_aux tl)
    | Inline.Plain s -> Inline.Plain (f s)
    | Inline.Link link ->
      let label = List.map inline_aux link.label in
      Inline.Link { link with label }
    | Inline.Subscript tl -> Inline.Subscript (List.map inline_aux tl)
    | Inline.Superscript tl -> Inline.Superscript (List.map inline_aux tl)
    | Inline.Footnote_Reference fr ->
      Inline.Footnote_Reference
        { fr with definition = Option.map (List.map inline_aux) fr.definition }
    | _ -> t
  in
  let rec block_list_aux list_item =
    let content' = List.map block_aux list_item.content in
    let items = List.map block_list_aux list_item.items in
    let name =
      List.map (fun (t', pos) -> (inline_aux t', pos)) list_item.name
    in
    { list_item with content = content'; items; name }
  and block_aux (t : Type.t) =
    match t with
    | Paragraph l ->
      Paragraph (List.map (fun (t', pos) -> (inline_aux t', pos)) l)
    | Heading heading ->
      let title' =
        List.map (fun (t', pos) -> (inline_aux t', pos)) heading.title
      in
      Heading { heading with title = title' }
    | List l -> List (List.map block_list_aux l)
    | Quote tl -> Quote (List.map block_aux tl)
    | Custom (name, opts, data, s) ->
      let data' = List.map block_aux data in
      Custom (name, opts, data', s)
    | Footnote_Definition (name, content) ->
      let content' = List.map (fun (t', pos) -> (inline_aux t', pos)) content in
      Footnote_Definition (name, content')
    | Table table ->
      let header = Option.map (List.map (List.map inline_aux)) table.header in
      let groups =
        List.map (List.map (List.map (List.map inline_aux))) table.groups
      in
      Table { table with header; groups }
    | _ -> t
  in
  block_aux t

(** unescape string in Type.Plain:
    e.g. \* -> *
    see also Parsers.md_escape_chars
    text in code fence should preserve '\' *)
let md_unescaped t = map_plain t unescaped_md_string

(** TODO *)
let md_escaped _t = failwith "not impl yet"

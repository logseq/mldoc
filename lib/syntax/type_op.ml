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

let inline_with_dummy_pos i = (i, Some Pos.dummy_pos)

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
  let len = String.length s in
  (* Fast path: no escapable backslash, return the string untouched. *)
  let rec needs_unescape i =
    if i + 1 >= len then
      false
    else if
      String.unsafe_get s i = '\\'
      && Parsers.is_md_escape_char (String.unsafe_get s (i + 1))
    then
      true
    else
      needs_unescape (i + 1)
  in
  if not (needs_unescape 0) then
    s
  else
    let b = of_string s in
    let n = ref 0 in
    let i = ref 0 in
    let lenb = length b in
    while !i < lenb do
      n :=
        !n
        +
        match get b !i with
        | '\\' when !i + 1 < lenb && Parsers.is_md_escape_char (get b (!i + 1))
          ->
          i := !i + 2;
          1
        | _ ->
          incr i;
          1
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

(* [map_share f l] maps [f] over [l] but returns the original list (and shares
   every tail) when [f] returns physically-equal results — avoids reallocating
   unchanged ASTs. *)
let rec map_share f l =
  match l with
  | [] -> l
  | x :: xs ->
    let x' = f x in
    let xs' = map_share f xs in
    if x' == x && xs' == xs then
      l
    else
      x' :: xs'

let map_escaped_string t f =
  let rec inline_aux (t : Inline.t) =
    match t with
    | Inline.Emphasis (em_type, tl) ->
      let tl' = map_share inline_aux tl in
      if tl' == tl then
        t
      else
        Inline.Emphasis (em_type, tl')
    | Inline.Tag tl ->
      let tl' = map_share inline_aux tl in
      if tl' == tl then
        t
      else
        Inline.Tag tl'
    | Inline.Plain s ->
      let s' = f s in
      if s' == s then
        t
      else
        Inline.Plain s'
    | Inline.Link link ->
      let label' = map_share inline_aux link.label in
      let url' =
        match link.url with
        | Inline.File s ->
          let s' = f s in
          if s' == s then
            link.url
          else
            Inline.File s'
        | Inline.Search s ->
          let s' = f s in
          if s' == s then
            link.url
          else
            Inline.Search s'
        | Inline.Page_ref s ->
          let s' = f s in
          if s' == s then
            link.url
          else
            Inline.Page_ref s'
        | Inline.Complex complex ->
          let link' = f complex.link in
          if link' == complex.link then
            link.url
          else
            Inline.Complex { complex with link = link' }
        | Inline.Block_ref _ -> link.url
        | Inline.Embed_data _ -> link.url
      in
      if label' == link.label && url' == link.url then
        t
      else
        Inline.Link { link with label = label'; url = url' }
    | Inline.Subscript tl ->
      let tl' = map_share inline_aux tl in
      if tl' == tl then
        t
      else
        Inline.Subscript tl'
    | Inline.Superscript tl ->
      let tl' = map_share inline_aux tl in
      if tl' == tl then
        t
      else
        Inline.Superscript tl'
    | Inline.Footnote_Reference fr ->
      let definition' =
        match fr.definition with
        | None -> fr.definition
        | Some l ->
          let l' = map_share inline_aux l in
          if l' == l then
            fr.definition
          else
            Some l'
      in
      if definition' == fr.definition then
        t
      else
        Inline.Footnote_Reference { fr with definition = definition' }
    | _ -> t
  in
  let inline_pos_aux (t', pos) =
    let t'' = inline_aux t' in
    if t'' == t' then
      (t', pos)
    else
      (t'', pos)
  in
  let rec block_list_aux list_item =
    let content' = map_share block_aux list_item.content in
    let items' = map_share block_list_aux list_item.items in
    let name' = map_share inline_pos_aux list_item.name in
    if
      content' == list_item.content
      && items' == list_item.items && name' == list_item.name
    then
      list_item
    else
      { list_item with content = content'; items = items'; name = name' }
  and block_aux (t : Type.t) =
    match t with
    | Paragraph l ->
      let l' = map_share inline_pos_aux l in
      if l' == l then
        t
      else
        Paragraph l'
    | Heading heading ->
      let title' = map_share inline_pos_aux heading.title in
      if title' == heading.title then
        t
      else
        Heading { heading with title = title' }
    | List l ->
      let l' = map_share block_list_aux l in
      if l' == l then
        t
      else
        List l'
    | Quote tl ->
      let tl' = map_share block_aux tl in
      if tl' == tl then
        t
      else
        Quote tl'
    | Custom (name, opts, data, s) ->
      let data' = map_share block_aux data in
      if data' == data then
        t
      else
        Custom (name, opts, data', s)
    | Footnote_Definition (name, content) ->
      let content' = map_share inline_pos_aux content in
      if content' == content then
        t
      else
        Footnote_Definition (name, content')
    | Table table ->
      let header' =
        match table.header with
        | None -> table.header
        | Some rows ->
          let rows' = map_share (map_share inline_aux) rows in
          if rows' == rows then
            table.header
          else
            Some rows'
      in
      let groups' =
        map_share (map_share (map_share (map_share inline_aux))) table.groups
      in
      if header' == table.header && groups' == table.groups then
        t
      else
        Table { table with header = header'; groups = groups' }
    | _ -> t
  in
  block_aux t

(** unescape string in Type.Plain:
    e.g. \* -> *
    see also Parsers.md_escape_chars
    text in code fence should preserve '\' *)
let md_unescaped t = map_escaped_string t unescaped_md_string

(** TODO *)
let md_escaped _t = failwith "not impl yet"

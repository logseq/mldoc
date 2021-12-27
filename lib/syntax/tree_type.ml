open! Prelude
open Option
module Z = Zip

(* type t = Type.t_with_pos_meta Z.t *)

type 'a t = 'a Z.t

type value = Type.t_with_pos_meta Z.l

type value_with_content = Type.t_with_content Z.l

(*
   - Heading (level=1)
       - Paragraph A
     - Paragraph B
     - Heading (level=2)
       - Paragraph C
   - Heading (level=1)

   convert to:

   [
       [
         Heading (level=1);
         [Paragraph A];
         [Paragraph B];
         [
             Heading (level=2);
             Paragraph C;
         ];
       ];
       [Heading (level=1)];
   ]

 *)

let insert_heading_block_exn block loc =
  let open Z in
  get_exn
    (insert_right loc ~item:(branch [ leaf block ]) >>= right >>= down)
    (Failure "insert_heading_block_exn")

let insert_normal_block_exn block loc =
  get_exn
    (Z.insert_right loc ~item:(Z.leaf block) >>= Z.right)
    (Failure "insert_normal_block_exn")

(** find the nearest parent heading, and goto rightmost of its children,
    if not found, goto rightmost of toplevel of the tree *)
let rec find_nearest_parent_heading loc level =
  let open Z in
  match node loc with
  | Leaf (Type.Heading h, _pos) -> (
    if h.unordered && h.level < level then
      rightmost loc
    else if (not h.unordered) && 1 < level then
      rightmost loc
    else
      match up loc with
      | None -> down loc >>| rightmost |> get
      | Some loc' -> find_nearest_parent_heading loc' level)
  | Leaf _ when List.length (lefts loc) > 0 ->
    find_nearest_parent_heading (leftmost loc) level
  | Leaf _ -> (
    match up loc with
    | None -> down loc >>| rightmost |> get
    | Some loc' -> find_nearest_parent_heading loc' level)
  | Branch _ when List.length (lefts loc) > 0 ->
    find_nearest_parent_heading (leftmost loc) level
  | Branch _ -> (
    match up loc with
    | None -> down loc >>| rightmost |> get
    | Some loc' -> find_nearest_parent_heading loc' level)

let add_heading_block_exn block loc =
  let type_t, _ = block in
  match type_t with
  | Type.Heading h ->
    let level =
      if h.unordered then
        h.level
      else
        1
    in
    find_nearest_parent_heading loc level |> insert_heading_block_exn block
  | _ -> failwith "unreachable"

let init_tree block =
  get_exn
    Z.(
      of_list [] |> append_child ~item:(branch [ leaf block ]) >>= down >>= down)
    (Failure "init_tree")

let of_blocks (blocks : Type.blocks) =
  match blocks with
  | [] -> Z.of_list []
  | h :: t ->
    let init = init_tree h in
    List.fold_left
      (fun loc block ->
        let type_t, _pos = block in
        match type_t with
        | Type.Heading _ -> add_heading_block_exn block loc
        | _ -> insert_normal_block_exn block loc)
      init t

let of_blocks_without_pos (blocks : Type.t list) =
  List.map
    (fun b -> (b, ({ start_pos = 0; end_pos = 0 } : Pos.pos_meta)))
    blocks
  |> of_blocks

let to_value = Z.root

let of_value = Z.of_l

let of_value_with_content = Z.of_l

let to_blocks t =
  let root_t = Z.of_l (to_value t) in
  let rec aux r t =
    if Z.is_end t then
      r
    else
      match Z.node t with
      | Z.Branch _ -> aux r (Z.next t)
      | Z.Leaf b -> aux (b :: r) (Z.next t)
  in
  aux [] root_t |> List.rev

let to_blocks_without_pos t = to_blocks t |> List.split |> fst

let to_blocks_with_content = to_blocks

(** operations on tree_type *)

let rec remove_properties (z : Type.t t) =
  let open Zip in
  let open Option in
  if is_end z then
    z
  else
    match node z with
    | Leaf (Type.Property_Drawer _) ->
      remove z |? z |> next |> remove_properties
    | _ -> remove_properties (next z)

(** replace (block|page)'s (embed|refs) *)

let heading_merely_have_embed ({ title; marker; priority; _ } : Type.heading) =
  match (Type_op.inline_list_strip_pos title, marker, priority) with
  | [ Macro hd ], None, None when hd.name = "embed" -> Some hd
  | _ -> None

let extract_macro_or_block_ref (il : Inline.t list) =
  List.find_map
    (fun i ->
      match i with
      | Inline.Macro m when m.name = "embed" -> Some (`Macro m)
      | Inline.Link { url = Inline.Block_ref s; _ } -> Some (`Block_ref s)
      | _ -> None)
    il

let macro_embed macro_argument (refs : Reference.parsed_t) =
  match macro_argument with
  | [] -> None
  | h :: _ ->
    let arg = String.trim h in
    let value =
      try String.(trim @@ sub arg 2 (length arg - 4)) with _ -> ""
    in
    if starts_with arg "[[" then
      let pagename = String.lowercase_ascii value in
      match List.assoc_opt pagename refs.parsed_embed_pages with
      | Some tl ->
        let t = of_blocks_without_pos tl |> to_value in
        Some t
      | None -> None
    else if starts_with arg "((" then
      let block_uuid = value in
      match List.assoc_opt block_uuid refs.parsed_embed_blocks with
      | Some (tl, _) ->
        let t = of_blocks_without_pos tl |> to_value in
        Some t
      | None -> None
    else
      None

let block_refs_embed arg (refs : Reference.parsed_t) =
  let block_uuid = String.trim arg in
  match List.assoc_opt block_uuid refs.parsed_embed_blocks with
  | Some (_, tl) ->
    Some
      (match tl with
      | Type.Heading h :: t -> Type.Paragraph h.title :: t
      | _ -> tl)
  | None -> None

(*
   1. [Heading [Macro]; Paragraph...]
         ^
      -> [[1st-expanded-macro-type-list]
          [2nd-expanded-macro-type-list]
          [<last-expanded-macro-type-list>; Paragraph...]]
   2. [Heading [Inline1;Macro;Inline2]; Paragraph...]
         ^
      -> [Heading [Inline1];
          <expanded-macro-type-list>;
          Paragraph [Inline2];
          Paragraph...]
 *)
let replace_only_embed t expanded_macro =
  match Z.up t with
  | None -> Z.replace t ~item:expanded_macro (* should not be here *)
  | Some t' -> (
    match expanded_macro with
    | Z.Branch l -> (
      let last, butlast = butlast l in
      let t'' =
        List.fold_left
          (fun t item ->
            match Z.insert_left t ~item with
            | None -> t
            | Some t' -> t')
          t' butlast
      in
      match last with
      | None -> t''
      | Some (Z.Leaf _ as last') ->
        let rights = Z.rights t in
        let t''' = Z.replace t'' ~item:(Z.branch (last' :: rights)) in
        Z.up t''' |? t'''
        (* return up t, we need to expand macros in macro*)
      | Some (Z.Branch last') ->
        let rights = Z.rights t in
        let t''' = Z.replace t'' ~item:(Z.branch (last' @ rights)) in
        Z.up t''' |? t'''
      (* return up t, we need to expand macros in macro*))
    | Z.Leaf _ as l ->
      let rights = Z.rights t in
      Z.replace t' ~item:(Z.branch (l :: rights)))

(** [Inline1;Inline2;Macro;Inline3]
   -> ([Inline1;Inline2], [Inline3]) *)
let split_by_macro inline_list macro =
  List.find_idx
    (fun inline ->
      match inline with
      | Inline.Macro macro' when macro' = macro -> true
      | _ -> false)
    inline_list
  >>| fun (index, _) ->
  (List.take index inline_list, List.drop (index + 1) inline_list)

let split_by_block_ref inline_list block_ref =
  List.find_idx
    (fun inline ->
      match inline with
      | Inline.Link { url = Inline.Block_ref s; _ } when s = block_ref -> true
      | _ -> false)
    inline_list
  >>| fun (index, _) ->
  (List.take index inline_list, List.drop (index + 1) inline_list)

let insert_right z ~expanded_macro =
  match expanded_macro with
  | Z.Branch items -> Z.insert_rights z ~items |? z
  | Z.Leaf _ as l -> Z.insert_right z ~item:l |? z

(* merge rightmost (paragraph or heading) of [lefts] with leftmost (paragraph & heading) of [mids].
   merge rightmost paragraph of [mids] with leftmost paragraph of [rights].*)
let merge_paragraphs_and_headings lefts mids rights =
  let rightmost_l, lefts_butlast = butlast lefts in
  let lefts', mids' =
    match (rightmost_l, mids) with
    | Some (Type.Paragraph inlines1), Type.Paragraph inlines2 :: t ->
      (lefts_butlast, Type.Paragraph (inlines1 @ inlines2) :: t)
    | Some (Type.Heading h), Type.Paragraph inlines :: t ->
      (lefts_butlast, Type.Heading { h with title = h.title @ inlines } :: t)
    | Some (Type.Paragraph inlines1), Type.Heading h :: t ->
      (lefts_butlast, Type.Paragraph (inlines1 @ h.title) :: t)
    | Some (Type.Heading h1), Type.Heading h2 :: t ->
      (lefts_butlast, Type.Heading { h1 with title = h1.title @ h2.title } :: t)
    | _ -> (lefts, mids)
  in
  let rightmost_m, mids_butlast = butlast mids' in
  let mids'', rights' =
    match (rightmost_m, rights) with
    | Some (Type.Paragraph inlines1), Type.Paragraph inlines2 :: t ->
      (mids_butlast, Type.Paragraph (inlines1 @ inlines2) :: t)
    | Some (Type.Heading h), Type.Paragraph inlines :: t ->
      (mids_butlast, Type.Heading { h with title = h.title @ inlines } :: t)
    | _ -> (mids', rights)
  in
  lefts' @ mids'' @ rights'

let replace_block_ref (t : Type.t_with_pos_meta Z.t) expanded_block_ref block_id
    =
  match Z.node t with
  | Branch _ -> t
  | Leaf (block, _pos) -> (
    match block with
    | Paragraph il -> (
      match split_by_block_ref (Type_op.inline_list_strip_pos il) block_id with
      | None -> t
      | Some (left, right) ->
        let lefts =
          [ Type.Paragraph (Type_op.inline_list_with_none_pos left) ]
        in
        let rights =
          [ Type.Paragraph (Type_op.inline_list_with_none_pos right) ]
        in
        let merged =
          merge_paragraphs_and_headings lefts expanded_block_ref rights
        in
        Z.insert_rights t
          ~items:(List.map (fun p -> Z.leaf (p, Pos.dummy_pos)) merged)
        >>= Z.remove |> default t)
    | Heading h -> (
      let ({ title; _ } : Type.heading) = h in
      match
        split_by_block_ref (Type_op.inline_list_strip_pos title) block_id
      with
      | None -> t
      | Some (left, right) ->
        let lefts =
          [ Type.Heading
              { h with title = Type_op.inline_list_with_none_pos left }
          ]
        in
        let rights =
          [ Type.Paragraph (Type_op.inline_list_with_none_pos right) ]
        in
        let merged =
          merge_paragraphs_and_headings lefts expanded_block_ref rights
        in
        Z.insert_rights t
          ~items:(List.map (fun p -> Z.leaf (p, Pos.dummy_pos)) merged)
        >>= Z.remove |> default t)
    | _ -> t)

let replace_macro (t : Type.t_with_pos_meta Z.t) expanded_macro macro =
  match Z.node t with
  | Branch _ -> t
  | Leaf (block, _pos) -> (
    match block with
    | Paragraph il -> (
      match split_by_macro (Type_op.inline_list_strip_pos il) macro with
      | None -> t
      | Some (left, right) ->
        Z.replace t
          ~item:
            (Z.leaf
               ( Type.Paragraph (Type_op.inline_list_with_none_pos left)
               , Pos.dummy_pos ))
        |> Z.insert_right
             ~item:
               (Z.leaf
                  ( Type.Paragraph (Type_op.inline_list_with_none_pos right)
                  , Pos.dummy_pos ))
        >>| insert_right ~expanded_macro
        |> default t)
    | Heading h -> (
      let ({ title; _ } : Type.heading) = h in
      match split_by_macro (Type_op.inline_list_strip_pos title) macro with
      | None -> t
      | Some (left, right) ->
        Z.replace t
          ~item:
            (Z.leaf
               ( Type.Heading
                   { h with title = Type_op.inline_list_with_none_pos left }
               , Pos.dummy_pos ))
        |> Z.insert_right
             ~item:
               (Z.leaf
                  ( Type.Paragraph (Type_op.inline_list_with_none_pos right)
                  , Pos.dummy_pos ))
        >>| insert_right ~expanded_macro
        |> default t)
    | Table _ -> t (* TODO: macro in table *)
    | _ -> t)

let rec replace_embed_and_refs (t : Type.t_with_pos_meta Z.t) ~refs =
  let root = Z.of_l (to_value t) in
  let rec aux z =
    if Z.is_end z then
      z
    else
      let z' = Z.next z in
      if List.length @@ Z.path z' > 100 then
        (* ignore when too deep *)
        aux z'
      else
        match Z.node z' with
        | Z.Branch _ -> aux z'
        | Z.Leaf (block, _pos) -> (
          match block with
          | Type.Heading h -> (
            match heading_merely_have_embed h with
            | Some embed -> (
              match macro_embed embed.arguments refs with
              | Some l ->
                let z'' = replace_only_embed z' l in
                aux z''
              | None -> aux z')
            | None ->
              let ({ title; _ } : Type.heading) = h in
              extract_macro_or_block_ref (Type_op.inline_list_strip_pos title)
              >>= (fun macro_or_block_ref ->
                    match macro_or_block_ref with
                    | `Macro macro ->
                      macro_embed macro.arguments refs >>= fun expanded_macro ->
                      return @@ replace_macro z' expanded_macro macro
                    | `Block_ref block_ref ->
                      block_refs_embed block_ref refs >>= fun expanded_macro ->
                      return
                      @@ replace_block_ref z'
                           (Type_op.remove_properties expanded_macro)
                           block_ref)
              |> default z' |> aux)
          | Type.Paragraph il ->
            extract_macro_or_block_ref (Type_op.inline_list_strip_pos il)
            >>= (fun macro_or_block_ref ->
                  match macro_or_block_ref with
                  | `Macro macro ->
                    macro_embed macro.arguments refs >>= fun expanded_macro ->
                    return @@ replace_macro z' expanded_macro macro
                  | `Block_ref block_ref ->
                    block_refs_embed block_ref refs >>= fun expanded_macro ->
                    return
                    @@ replace_block_ref z'
                         (Type_op.remove_properties expanded_macro)
                         block_ref)
            |> default z' |> aux
          | Type.Quote tl ->
            let t = of_blocks_without_pos tl in
            let t' = replace_embed_and_refs t ~refs in
            let tl' = to_blocks_without_pos t' in
            Z.replace z' ~item:(Z.Leaf (Type.Quote tl', Pos.dummy_pos)) |> aux
          | Type.List items ->
            let rec list_aux items =
              List.map
                (fun (item : Type.list_item) ->
                  let nested_items_t = list_aux item.items in
                  let content =
                    replace_embed_and_refs
                      (of_blocks_without_pos item.content)
                      ~refs
                    |> to_blocks_without_pos
                  in
                  { item with content; items = nested_items_t })
                items
            in
            Z.replace z'
              ~item:(Z.Leaf (Type.List (list_aux items), Pos.dummy_pos))
            |> aux
          | Type.Footnote_Definition _
          | Type.Table _ ->
            aux z' (* TODO: replace macro|ref in table & footnote_definition *)
          | _ -> aux z')
  in
  aux root

let replace_heading_with_paragraph (t : Type.t_with_pos_meta Z.t) =
  let root = Z.of_l (to_value t) in
  let rec aux z =
    if Z.is_end z then
      z
    else
      let z' = Z.next z in
      match Z.node z' with
      | Z.Leaf (Type.Heading h, _pos) ->
        let paragraph =
          let r = h.title in
          let r =
            Option.map_default
              (fun priority ->
                Type_op.inline_with_dummy_pos
                  (Inline.Plain (Util.priority_to_string priority ^ " "))
                :: r)
              r h.priority
          in
          let r =
            Option.map_default
              (fun marker ->
                Type_op.inline_with_dummy_pos (Inline.Plain (marker ^ " ")) :: r)
              r h.marker
          in
          let r =
            Option.map_default
              (fun size ->
                Type_op.inline_with_dummy_pos
                  (Inline.Plain (String.make size '#' ^ " "))
                :: r)
              r h.size
          in
          Type.Paragraph r
        in
        aux @@ Z.replace z' ~item:(Z.Leaf (paragraph, Pos.dummy_pos))
      | _ -> aux z'
  in
  aux root

let flatten (t : Type.t_with_pos_meta Z.t) =
  let value = to_value t in
  let rec aux vl =
    List.concat_map
      (function
        | Z.Branch l -> aux l
        | Z.Leaf _ as v -> [ v ])
      vl
  in
  let value' =
    match value with
    | Z.Branch l -> aux l
    | Z.Leaf _ as v -> aux [ v ]
  in
  of_value (Z.Branch value')

let rec nested_link_to_plain ({ children; _ } : Nested_link.t) =
  String.concat ""
  @@ List.map
       (fun child ->
         match child with
         | Nested_link.Label s -> s
         | Nested_link.Nested_link t_with_pos ->
           nested_link_to_plain (fst t_with_pos))
       children

let remove_meta_chars_internal remove_emphasis remove_page_ref
    (inlines : Inline.t list) : Inline.t list =
  let rec aux inlines =
    let open Inline in
    List.fold_right
      (fun inline r ->
        match inline with
        | Emphasis (em, tl) ->
          if remove_emphasis then
            aux tl @ r
          else
            Emphasis (em, aux tl) :: r
        | Link { url; label; _ } when remove_page_ref -> (
          match (url, label) with
          | Page_ref s, []
          | Page_ref s, [ Plain "" ] ->
            Plain s :: r
          | _ -> inline :: r)
        | Tag tl -> aux tl @ r
        | Nested_link nl when remove_page_ref ->
          Plain (nested_link_to_plain nl) :: r
        | Subscript tl -> Subscript (aux tl) :: r
        | Superscript tl -> Superscript (aux tl) :: r
        | Footnote_Reference { id; name; definition } ->
          (match definition with
          | Some tl ->
            Footnote_Reference { id; name; definition = Some (aux tl) }
          | None -> inline)
          :: r
        | _ -> inline :: r)
      inlines []
  in
  aux inlines |> Inline.concat_plains_without_pos

let rec remove_meta_chars_internal2 remove_emphasis remove_page_ref (t : Type.t)
    : Type.t =
  let remove_meta_chars_internal' =
    remove_meta_chars_internal remove_emphasis remove_page_ref
  in
  let list_map_remove_meta_chars_internal2 =
    List.map (remove_meta_chars_internal2 remove_emphasis remove_page_ref)
  in
  match t with
  | Type.Paragraph tl_with_pos ->
    let tl = List.map fst tl_with_pos |> remove_meta_chars_internal' in
    Type.Paragraph (Type_op.inline_list_with_dummy_pos tl)
  | Type.Heading h ->
    Type.Heading
      { h with
        title =
          Type_op.inline_list_with_dummy_pos
          @@ remove_meta_chars_internal' (List.map fst h.title)
      }
  | Type.List l ->
    let rec list_item_f (list_item : Type.list_item) =
      { list_item with
        content = list_map_remove_meta_chars_internal2 list_item.content
      ; items = List.map list_item_f list_item.items
      ; name =
          remove_meta_chars_internal' (List.map fst list_item.name)
          |> Type_op.inline_list_with_dummy_pos
      }
    in
    Type.List (List.map list_item_f l)
  | Type.Quote tl -> Type.Quote (list_map_remove_meta_chars_internal2 tl)
  | Type.Footnote_Definition (s, tl_with_pos) ->
    Type.Footnote_Definition
      ( s
      , Type_op.inline_list_with_dummy_pos
        @@ remove_meta_chars_internal' (List.map fst tl_with_pos) )
  | Type.Table { header; groups; col_groups } ->
    let header' =
      match header with
      | None -> None
      | Some h -> Some (List.map remove_meta_chars_internal' h)
    in
    let groups' =
      List.map (List.map (List.map remove_meta_chars_internal')) groups
    in
    Type.Table { header = header'; groups = groups'; col_groups }
  | _ -> t

let remove_meta_chars_aux (meta_chars : Conf.meta_chars list)
    (t : Type.t_with_pos_meta Z.t) =
  let remove_emphasis = List.mem Conf.Emphasis meta_chars in
  let remove_page_ref = List.mem Conf.Page_ref meta_chars in
  let root = Z.of_l (to_value t) in
  let rec aux z =
    if Z.is_end z then
      z
    else
      let z' = Z.next z in
      match Z.node z' with
      | Z.Branch _ -> aux z'
      | Z.Leaf (block, pos) ->
        Z.replace z'
          ~item:
            (Z.leaf
               ( remove_meta_chars_internal2 remove_emphasis remove_page_ref
                   block
               , pos ))
        |> aux
  in
  aux root

let remove_meta_chars (meta_chars : Conf.meta_chars list)
    (t : Type.t_with_pos_meta Z.t) =
  if List.length meta_chars = 0 then
    t
  else
    remove_meta_chars_aux meta_chars t

open! Prelude
open Option
module Z = Zip

type t = Type.t_with_pos_meta Z.t

type value = Type.t_with_pos_meta Z.l

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
    (fun b -> (b, ({ start_pos = 0; end_pos = 0 } : Type.pos_meta)))
    blocks
  |> of_blocks

let to_value = Z.root

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

(** replace (block|page)'s (embed|refs) *)

let heading_merely_have_embed ({ title; marker; priority; _ } : Type.heading) =
  match (title, marker, priority) with
  | [ Macro hd ], None, None when hd.name = "embed" -> Some hd
  | _ -> None

let extract_macro_or_block_ref (il : Inline.t list) =
  List.find_map
    (fun i ->
      match i with
      | Inline.Macro m when m.name = "embed" -> Some (`Macro m)
      | Inline.Block_reference s -> Some (`Block_ref s)
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
      let pagename = value in
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
    let t = of_blocks_without_pos tl |> Z.root |> Z.of_l in
    let t =
      to_value
      @@ Z.edit t ~f:(fun e ->
             match e with
             | Z.Branch (Z.Branch (Z.Leaf (Type.Heading h, _) :: tail) :: tail')
               ->
               Z.Branch
                 (Z.Branch
                    (Z.Leaf (Type.Paragraph h.title, Type.dummy_pos) :: tail)
                 :: tail')
             | _ -> e)
    in
    Some t
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
  CCList.find_idx
    (fun inline ->
      match inline with
      | Inline.Macro macro' when macro' = macro -> true
      | _ -> false)
    inline_list
  >>| fun (index, _) ->
  (CCList.take index inline_list, CCList.drop (index + 1) inline_list)

let split_by_block_ref inline_list block_ref =
  CCList.find_idx
    (fun inline ->
      match inline with
      | Inline.Block_reference s when s = block_ref -> true
      | _ -> false)
    inline_list
  >>| fun (index, _) ->
  (CCList.take index inline_list, CCList.drop (index + 1) inline_list)

let insert_right z ~expanded_macro =
  match expanded_macro with
  | Z.Branch items -> Z.insert_rights z ~items |? z
  | Z.Leaf _ as l -> Z.insert_right z ~item:l |? z

let insert_right_block_ref z ~expanded_block_ref =
  match expanded_block_ref with
  | Z.Branch items -> Z.insert_rights z ~items |? z
  | Z.Leaf _ as l -> Z.insert_right z ~item:l |? z

let replace_block_ref (t : Type.t_with_pos_meta Z.t) expanded_block_ref block_id
    =
  match Z.node t with
  | Branch _ -> t
  | Leaf (block, _pos) -> (
    match block with
    | Paragraph il -> (
      match split_by_block_ref il block_id with
      | None -> t
      | Some (left, right) ->
        Z.replace t ~item:(Z.leaf (Type.Paragraph left, Type.dummy_pos))
        |> Z.insert_right ~item:(Z.leaf (Type.Paragraph right, Type.dummy_pos))
        >>| insert_right_block_ref ~expanded_block_ref
        |> default t)
    | Heading h -> (
      let ({ title; _ } : Type.heading) = h in
      match split_by_block_ref title block_id with
      | None -> t
      | Some (left, right) ->
        Z.replace t
          ~item:(Z.leaf (Type.Heading { h with title = left }, Type.dummy_pos))
        |> Z.insert_right ~item:(Z.leaf (Type.Paragraph right, Type.dummy_pos))
        >>| insert_right_block_ref ~expanded_block_ref
        |> default t)
    | _ -> t)

let replace_macro (t : Type.t_with_pos_meta Z.t) expanded_macro macro =
  match Z.node t with
  | Branch _ -> t
  | Leaf (block, _pos) -> (
    match block with
    | Paragraph il -> (
      match split_by_macro il macro with
      | None -> t
      | Some (left, right) ->
        Z.replace t ~item:(Z.leaf (Type.Paragraph left, Type.dummy_pos))
        |> Z.insert_right ~item:(Z.leaf (Type.Paragraph right, Type.dummy_pos))
        >>| insert_right ~expanded_macro
        |> default t)
    | Heading h -> (
      let ({ title; _ } : Type.heading) = h in
      match split_by_macro title macro with
      | None -> t
      | Some (left, right) ->
        Z.replace t
          ~item:(Z.leaf (Type.Heading { h with title = left }, Type.dummy_pos))
        |> Z.insert_right ~item:(Z.leaf (Type.Paragraph right, Type.dummy_pos))
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
            extract_macro_or_block_ref title
            >>= (fun macro_or_block_ref ->
                  match macro_or_block_ref with
                  | `Macro macro ->
                    macro_embed macro.arguments refs >>= fun expanded_macro ->
                    return @@ replace_macro z' expanded_macro macro
                  | `Block_ref block_ref ->
                    block_refs_embed block_ref refs >>= fun expanded_macro ->
                    return @@ replace_block_ref z' expanded_macro block_ref)
            |> default z' |> aux)
        | Type.Paragraph il ->
          extract_macro_or_block_ref il
          >>= (fun macro_or_block_ref ->
                match macro_or_block_ref with
                | `Macro macro ->
                  macro_embed macro.arguments refs >>= fun expanded_macro ->
                  return @@ replace_macro z' expanded_macro macro
                | `Block_ref block_ref ->
                  block_refs_embed block_ref refs >>= fun expanded_macro ->
                  return @@ replace_block_ref z' expanded_macro block_ref)
          |> default z' |> aux
        | Type.Quote tl ->
          let t = of_blocks_without_pos tl in
          let t' = replace_embed_and_refs t ~refs in
          let tl' = to_blocks_without_pos t' in
          Z.replace z' ~item:(Z.Leaf (Type.Quote tl', Type.dummy_pos)) |> aux
        | Type.List items ->
          let rec list_aux items =
            CCList.map
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
            ~item:(Z.Leaf (Type.List (list_aux items), Type.dummy_pos))
          |> aux
        | Type.Footnote_Definition _
        | Type.Table _ ->
          aux z' (* TODO: replace macro|ref in table & footnote_definition *)
        | _ -> aux z')
  in
  aux root

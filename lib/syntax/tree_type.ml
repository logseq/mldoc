open! Prelude
open Option
module Z = Zip

type t = Type.t_with_pos_meta Z.t

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

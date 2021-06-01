open Prelude
open Option

type 'a l =
  | Leaf of 'a
  | Branch of 'a l list

type 'a ppath =
  { left : 'a l list
  ; right : 'a l list
  ; pnodes : 'a l list
  ; ppath : 'a ppath option
  ; changed : bool
  }

type 'a t =
  { value : 'a l
  ; ppath : 'a ppath option
  ; left : 'a l list
  ; right : 'a l list
  ; pnodes : 'a l list
  ; changed : bool
  ; end' : bool
  }

(* internal helpers *)

let leaf a = Leaf a

let branch a = Branch a

let bool_to_option = function
  | true -> Some ()
  | false -> None

let list_to_option l =
  match l with
  | [] -> None
  | h :: t -> Some (h, t)

let butlast l =
  match List.rev l with
  | [] -> (None, [])
  | [ h ] -> (Some h, [])
  | h :: t -> (Some h, List.rev t)

let rec concat_rev l_rev r =
  match l_rev with
  | [] -> r
  | h :: t -> concat_rev t (h :: r)

(* APIs *)

let of_l (value : 'a l) =
  { value
  ; ppath = None
  ; left = []
  ; right = []
  ; pnodes = []
  ; changed = false
  ; end' = false
  }

let of_list l =
  let value = branch @@ CCList.map (fun e -> leaf e) l in
  of_l value

let node t = t.value

let is_branch t =
  match node t with
  | Branch _ -> true
  | Leaf _ -> false

let children t =
  if is_branch t then
    match node t with
    | Branch l -> Some l
    | Leaf _ -> failwith "unreachable"
  else
    None

let children_exn t =
  match children t with
  | Some v -> v
  | None -> failwith "called children on a leaf node"

let path t = t.pnodes

let lefts t = t.left

let rights t = t.right

let down t =
  bool_to_option (not t.end') >>= fun () ->
  bool_to_option (is_branch t) >>= fun () ->
  let node = node t in
  let cs = get (children t) in
  match cs with
  | c :: cnext ->
    let value = c in
    let left = [] in
    let right = cnext in
    let pnodes = node :: t.pnodes in
    let ppath' =
      { left = t.left
      ; right = t.right
      ; pnodes = t.pnodes
      ; changed = t.changed
      ; ppath = t.ppath
      }
    in
    Some
      { value
      ; left
      ; right
      ; pnodes
      ; changed = false
      ; ppath = Some ppath'
      ; end' = false
      }
  | [] -> None

let up t =
  bool_to_option (not t.end') >>= fun () ->
  list_to_option t.pnodes >>= fun (hnode, _) ->
  let left = map_default (fun (ppath : 'a ppath) -> ppath.left) [] t.ppath in
  let right = map_default (fun (ppath : 'a ppath) -> ppath.right) [] t.ppath in
  let pnodes =
    map_default (fun (ppath : 'a ppath) -> ppath.pnodes) [] t.ppath
  in
  let ppath =
    map_default (fun (ppath : 'a ppath) -> ppath.ppath) None t.ppath
  in
  if t.changed then
    let value = branch @@ List.concat [ List.rev t.left; t.value :: t.right ] in
    let changed = t.changed in
    Some { value; left; right; pnodes; changed; ppath; end' = false }
  else
    let value = hnode in
    let changed =
      map_default (fun (ppath : 'a ppath) -> ppath.changed) false t.ppath
    in
    Some { value; left; right; pnodes; changed; ppath; end' = false }

let rec root t =
  if t.end' then
    node t
  else
    match up t with
    | Some up_t -> root up_t
    | None -> node t

let right t =
  bool_to_option (not t.end') >>= fun () ->
  list_to_option t.right >>= fun (r, rs) ->
  let left = t.value :: t.left in
  let value = r in
  let right = rs in
  Some { t with value; left; right }

(** Returns the loc of the rightmost sibling of the node at this loc, or self *)
let rightmost t =
  let last, butlast = butlast t.right in
  match last with
  | None -> t
  | Some last' ->
    let value = last' in
    let left = concat_rev butlast (t.value :: t.left) in
    let right = [] in
    { t with value; left; right }

let left t =
  bool_to_option (not t.end') >>= fun () ->
  list_to_option t.left >>= fun (l, ls) ->
  let left = ls in
  let value = l in
  let right = t.value :: t.right in
  Some { t with value; left; right }

(** Returns the loc of the leftmost sibling of the node at this loc, or self *)
let leftmost t =
  let last, butlast = butlast t.left in
  match last with
  | None -> t
  | Some last' ->
    let value = last' in
    let left = [] in
    let right = concat_rev butlast (t.value :: t.right) in
    { t with value; left; right }

let insert_left t ~item =
  bool_to_option (not t.end') >>= fun () ->
  list_to_option t.pnodes >>= fun _ ->
  let left = item :: t.left in
  Some { t with left; changed = true }

let insert_right t ~item =
  bool_to_option (not t.end') >>= fun () ->
  list_to_option t.pnodes >>= fun _ ->
  let right = item :: t.right in
  Some { t with right; changed = true }

let replace t ~item = { t with value = item; changed = true }

let edit t ~f = replace t ~item:(f (node t))

(** Inserts the item as the leftmost child of the node at this loc, without moving  *)
let insert_child t ~item =
  children t >>= fun children ->
  some @@ replace t ~item:(branch (item :: children))

(** Inserts the item as the rightmost child of the node at this loc, without moving *)
let append_child t ~item =
  children t >>= fun children ->
  some @@ replace t ~item:(branch (List.append children [ item ]))

let rec next_up_aux t =
  match up t with
  | Some t' -> (
    match right t' with
    | Some t'' -> t''
    | None -> next_up_aux t')
  | None ->
    { value = node t
    ; ppath = None
    ; left = []
    ; right = []
    ; pnodes = []
    ; changed = false
    ; end' = true
    }

let next t =
  if t.end' then
    t
  else
    match down t with
    | Some t' -> t'
    | None -> (
      match right t with
      | Some t' -> t'
      | None -> next_up_aux t)

let rec prev_aux t =
  match down t with
  | Some t' -> prev_aux (rightmost t')
  | None -> t

let prev t =
  match left t with
  | Some t' -> some @@ prev_aux t'
  | None -> up t

let is_end t = t.end'

(** Removes the node at loc, returning the loc that would have preceded it in a depth-first walk. *)
let remove t =
  (* not at top level *)
  list_to_option t.pnodes >>= fun _ ->
  t.ppath >>= fun ppath ->
  match t.left with
  | l :: ls ->
    let left = ls in
    let value = l in
    let changed = true in
    some @@ prev_aux { t with left; value; changed }
  | [] ->
    let value = branch t.right in
    let changed = true in
    let left = ppath.left in
    let right = ppath.right in
    let ppath' = ppath.ppath in
    let pnodes = ppath.pnodes in
    some @@ { t with value; left; right; changed; pnodes; ppath = ppath' }

open! Prelude
open Angstrom
open Parsers

(** Outline mode only extracts node refs and tags (properties are block-level). *)
let is_outline_special = function
  | '#'
  | '['
  | '(' ->
    true
  | _ -> false

let may_have_outline_markup_range s off len =
  let end_ = off + len in
  let i = ref off in
  let found = ref false in
  while (not !found) && !i < end_ do
    if is_outline_special (String.unsafe_get s !i) then
      found := true
    else
      incr i
  done;
  !found

let may_have_outline_markup _config s =
  may_have_outline_markup_range s 0 (String.length s)

let skip_plain_run =
  skip_while (fun c -> (not (is_outline_special c)) && not (is_whitespace c))

let interesting config : Inline.t Angstrom.t =
  peek_char_fail >>= function
  | '#' -> Inline.hash_tag config
  | '[' -> Inline.nested_link_or_link config
  | '(' -> Inline.block_reference config
  | _ -> fail "not outline inline"

let inline_choices config : Inline.t_with_pos option Angstrom.t =
  peek_char_fail >>= function
  | c when is_whitespace c -> any_char *> return None
  | _ ->
    interesting config
    >>| (fun t -> Some (t, None))
    <|> any_char *> skip_plain_run *> return None

let parse_angstrom config =
  many1 (inline_choices config)
  >>| (fun l ->
        let l = List.filter_map (fun x -> x) l in
        Inline.concat_plains l)
  <?> "outline inline"

let is_ws = function
  | ' '
  | '\t'
  | '\n'
  | '\r' ->
    true
  | _ -> false

let tag_trail = function
  | ','
  | ';'
  | '.'
  | '!'
  | '?'
  | '\''
  | '"'
  | ':'
  | '#' ->
    true
  | _ -> false

(** Iterative scan; returns [(end_pos, has_inner_page_ref)]. *)
let find_page_ref_end s i =
  let n = String.length s in
  if i + 1 >= n || s.[i] <> '[' || s.[i + 1] <> '[' then
    None
  else
    let j = ref (i + 2) in
    let depth = ref 1 in
    let inner = ref false in
    let found = ref None in
    while !found = None && !j + 1 < n do
      let c = String.unsafe_get s !j in
      if c = '[' && String.unsafe_get s (!j + 1) = '[' then (
        incr depth;
        inner := true;
        j := !j + 2
      ) else if c = ']' && String.unsafe_get s (!j + 1) = ']' then (
        decr depth;
        if !depth = 0 then
          found := Some (!j + 2)
        else
          j := !j + 2
      ) else
        incr j
    done;
    match !found with
    | Some e -> Some (e, !inner)
    | None -> None

let find_block_ref_end s i =
  let n = String.length s in
  if i + 1 >= n || s.[i] <> '(' || s.[i + 1] <> '(' then
    None
  else
    let j = ref (i + 2) in
    let found = ref None in
    while !found = None && !j + 1 < n do
      if String.unsafe_get s !j = ')' && String.unsafe_get s (!j + 1) = ')' then
        found := Some (!j + 2)
      else
        incr j
    done;
    !found

let empty_plain = [ Inline.Plain "" ]
let max_nested_ref_depth = 32

type 'a rtab =
  { mutable data : (string * 'a) list array
  ; mutable count : int
  }

let rtab_create n = { data = Array.make n []; count = 0 }

let hash_range s off len =
  let h = ref 0 in
  for i = 0 to len - 1 do
    h :=
      (!h * 31 + Char.code (String.unsafe_get s (off + i))) land 0x3fffffff
  done;
  !h

let range_eq s off key =
  let n = String.length key in
  let i = ref 0 in
  let ok = ref true in
  while !ok && !i < n do
    if String.unsafe_get s (off + !i) <> String.unsafe_get key !i then
      ok := false
    else
      incr i
  done;
  !ok

let rtab_find tbl s off len =
  let i = hash_range s off len land (Array.length tbl.data - 1) in
  let rec look = function
    | [] -> None
    | (k, v) :: rest ->
      if String.length k = len && range_eq s off k then
        Some v
      else
        look rest
  in
  look tbl.data.(i)

let rtab_add tbl key v =
  let n = Array.length tbl.data in
  if tbl.count > n then (
    let n' = n * 2 in
    let data' = Array.make n' [] in
    Array.iter
      (fun bucket ->
        List.iter
          (fun ((k, _) as pair) ->
            let i = hash_range k 0 (String.length k) land (n' - 1) in
            data'.(i) <- pair :: data'.(i))
          bucket)
      tbl.data;
    tbl.data <- data'
  );
  let i = hash_range key 0 (String.length key) land (Array.length tbl.data - 1) in
  tbl.data.(i) <- (key, v) :: tbl.data.(i);
  tbl.count <- tbl.count + 1

type intern =
  { pages : Inline.t rtab
  ; tags : Inline.t rtab
  ; blocks : Inline.t rtab
  ; titles : (Inline.t * Inline.t, Inline.t_with_pos list) Hashtbl.t
  }

let make_intern () =
  { pages = rtab_create 256
  ; tags = rtab_create 64
  ; blocks = rtab_create 256
  ; titles = Hashtbl.create 1024
  }

let current_intern : intern option ref = ref None

let with_intern f =
  current_intern := Some (make_intern ());
  Fun.protect ~finally:(fun () -> current_intern := None) f

let page_ref_link_fresh name full_text =
  Inline.Link
    { url = Inline.Page_ref name
    ; label = empty_plain
    ; title = None
    ; full_text
    ; metadata = ""
    }

let page_ref_link name =
  match !current_intern with
  | Some t -> (
    match rtab_find t.pages name 0 (String.length name) with
    | Some v -> v
    | None ->
      let v = page_ref_link_fresh name (Printf.sprintf "[[%s]]" name) in
      rtab_add t.pages name v;
      v)
  | None -> page_ref_link_fresh name (Printf.sprintf "[[%s]]" name)

let page_ref_link_range s start end_ =
  let name_off = start + 2 in
  let name_len = end_ - start - 4 in
  match !current_intern with
  | Some t -> (
    match rtab_find t.pages s name_off name_len with
    | Some v -> v
    | None ->
      let name = String.sub s name_off name_len in
      let full_text = String.sub s start (end_ - start) in
      let v = page_ref_link_fresh name full_text in
      rtab_add t.pages name v;
      v)
  | None ->
    let name = String.sub s name_off name_len in
    let full_text = String.sub s start (end_ - start) in
    page_ref_link_fresh name full_text

let block_ref_link_fresh id full_text =
  Inline.Link
    { url = Inline.Block_ref id
    ; label = empty_plain
    ; title = None
    ; full_text
    ; metadata = ""
    }

let block_ref_link id =
  match !current_intern with
  | Some t -> (
    match rtab_find t.blocks id 0 (String.length id) with
    | Some v -> v
    | None ->
      let v = block_ref_link_fresh id (Printf.sprintf "((%s))" id) in
      rtab_add t.blocks id v;
      v)
  | None -> block_ref_link_fresh id (Printf.sprintf "((%s))" id)

let block_ref_link_range s start end_ =
  let id_off = start + 2 in
  let id_len = end_ - start - 4 in
  match !current_intern with
  | Some t -> (
    match rtab_find t.blocks s id_off id_len with
    | Some v -> v
    | None ->
      let id = String.sub s id_off id_len in
      let full_text = String.sub s start (end_ - start) in
      let v = block_ref_link_fresh id full_text in
      rtab_add t.blocks id v;
      v)
  | None ->
    let id = String.sub s id_off id_len in
    let full_text = String.sub s start (end_ - start) in
    block_ref_link_fresh id full_text

let tag_of_name name =
  match !current_intern with
  | Some t -> (
    match rtab_find t.tags name 0 (String.length name) with
    | Some v -> v
    | None ->
      let v = Inline.Tag [ Inline.Plain name ] in
      rtab_add t.tags name v;
      v)
  | None -> Inline.Tag [ Inline.Plain name ]

let tag_of_range s off len =
  match !current_intern with
  | Some t -> (
    match rtab_find t.tags s off len with
    | Some v -> v
    | None ->
      let name = String.sub s off len in
      let v = Inline.Tag [ Inline.Plain name ] in
      rtab_add t.tags name v;
      v)
  | None -> Inline.Tag [ Inline.Plain (String.sub s off len) ]

let strip_tag_trail raw =
  let n = String.length raw in
  let j = ref n in
  while !j > 0 && tag_trail raw.[!j - 1] do
    decr j
  done;
  if !j = n then
    raw
  else if !j = 0 then
    ""
  else
    String.sub raw 0 !j

let rec build_nested_link s start end_ depth =
  if depth > max_nested_ref_depth then
    None
  else
    let content = String.sub s start (end_ - start) in
    let inner_off = start + 2 in
    let inner_end = end_ - 2 in
    let children = ref [] in
    let i = ref inner_off in
    let ok = ref true in
    while !i < inner_end && !ok do
      if !i + 1 < inner_end && s.[!i] = '[' && s.[!i + 1] = '[' then (
        match find_page_ref_end s !i with
        | Some (e, _) when e <= inner_end -> (
          match build_nested_link s !i e (depth + 1) with
          | Some nl ->
            children := Nested_link.Nested_link (nl, None) :: !children;
            i := e
          | None -> ok := false)
        | _ -> ok := false
      ) else
        let j = ref !i in
        while
          !j < inner_end
          && not (!j + 1 < inner_end && s.[!j] = '[' && s.[!j + 1] = '[')
        do
          incr j
        done;
        if !j > !i then (
          children := Nested_link.Label (String.sub s !i (!j - !i)) :: !children;
          i := !j
        ) else
          ok := false
    done;
    if (not !ok) || !children = [] then
      None
    else
      Some { Nested_link.content; children = List.rev !children }

(** Fast path for #tag / [[page]] / ((block)) / nested [[a [[b]]]].
    Returns None when markdown links or nested-page hashtags need Angstrom.
    Scans [s] from [off] with length [len] (no need to sub the whole title). *)
let has_backslash s off end_ =
  let i = ref off in
  let found = ref false in
  while (not !found) && !i < end_ do
    if String.unsafe_get s !i = '\\' then
      found := true
    else
      incr i
  done;
  !found

let try_fast_scan_range s off len =
  if len < 0 || off < 0 || off + len > String.length s then
    None
  else if has_backslash s off (off + len) then
    None
  else
    let end_ = off + len in
    let acc = ref [] in
    let i = ref off in
    let complex = ref false in
    while !i < end_ && not !complex do
      match String.unsafe_get s !i with
      | '\\' -> complex := true
      | '#' when !i + 1 < end_ && (not (is_ws s.[!i + 1])) && s.[!i + 1] <> '#'
        ->
        let start = !i + 1 in
        let j = ref start in
        let has_bracket = ref false in
        while !j < end_ && not (is_ws (String.unsafe_get s !j)) do
          if String.unsafe_get s !j = '[' then has_bracket := true;
          incr j
        done;
        if !has_bracket then
          complex := true
        else
          let k = ref !j in
          while !k > start && tag_trail s.[!k - 1] do
            decr k
          done;
          if !k > start then acc := (tag_of_range s start (!k - start), None) :: !acc;
          i := !j
      | '[' when !i + 1 < end_ && s.[!i + 1] = '[' -> (
        match find_page_ref_end s !i with
        | Some (e, inner) when e <= end_ ->
          if inner then (
            match build_nested_link s !i e 0 with
            | Some nl when List.length nl.children > 1 ->
              acc := (Inline.Nested_link nl, None) :: !acc;
              i := e
            | Some _ ->
              acc := (page_ref_link_range s !i e, None) :: !acc;
              i := e
            | None -> complex := true
          ) else (
            acc := (page_ref_link_range s !i e, None) :: !acc;
            i := e
          )
        | _ -> complex := true)
      | '[' -> complex := true
      | '(' when !i + 1 < end_ && s.[!i + 1] = '(' -> (
        match find_block_ref_end s !i with
        | Some e when e <= end_ ->
          acc := (block_ref_link_range s !i e, None) :: !acc;
          i := e
        | _ -> incr i)
      | _ -> incr i
    done;
    if !complex then
      None
    else
      match (!current_intern, !acc) with
      | Some t, [ (q, None); (p, None) ] -> (
        try Some (Hashtbl.find t.titles (p, q)) with
        | Not_found ->
          let result = [ (p, None); (q, None) ] in
          Hashtbl.add t.titles (p, q) result;
          Some result)
      | _ -> Some (List.rev !acc)

let try_fast_scan s = try_fast_scan_range s 0 (String.length s)

let parse config =
  take_while (fun _ -> true) >>= fun s ->
  match try_fast_scan s with
  | Some result -> return result
  | None -> (
    match parse_string ~consume:All (parse_angstrom config) s with
    | Ok result -> return result
    | Error e -> fail e)

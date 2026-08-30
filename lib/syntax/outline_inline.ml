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

let may_have_outline_markup _config s =
  let n = String.length s in
  let i = ref 0 in
  let found = ref false in
  while (not !found) && !i < n do
    if is_outline_special (String.unsafe_get s !i) then
      found := true
    else
      incr i
  done;
  !found

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

let page_ref_link name =
  Inline.Link
    { url = Inline.Page_ref name
    ; label = empty_plain
    ; title = None
    ; full_text = Printf.sprintf "[[%s]]" name
    ; metadata = ""
    }

let block_ref_link id =
  Inline.Link
    { url = Inline.Block_ref id
    ; label = empty_plain
    ; title = None
    ; full_text = Printf.sprintf "((%s))" id
    ; metadata = ""
    }

let strip_tag_trail raw =
  let rec strip t =
    let len = String.length t in
    if len = 0 then
      t
    else if tag_trail t.[len - 1] then
      strip (String.sub t 0 (len - 1))
    else
      t
  in
  strip raw

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
let try_fast_scan_range s off len =
  if len < 0 || off < 0 || off + len > String.length s then
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
          let name = strip_tag_trail (String.sub s start (!j - start)) in
          if name <> "" then
            acc := (Inline.Tag [ Inline.Plain name ], None) :: !acc;
          i := !j
      | '[' when !i + 1 < end_ && s.[!i + 1] = '[' -> (
        match find_page_ref_end s !i with
        | Some (e, inner) when e <= end_ ->
          let name = String.sub s (!i + 2) (e - !i - 4) in
          if inner then (
            match build_nested_link s !i e 0 with
            | Some nl when List.length nl.children > 1 ->
              acc := (Inline.Nested_link nl, None) :: !acc;
              i := e
            | Some _ ->
              acc := (page_ref_link name, None) :: !acc;
              i := e
            | None -> complex := true
          ) else (
            acc := (page_ref_link name, None) :: !acc;
            i := e
          )
        | _ -> complex := true)
      | '[' -> complex := true
      | '(' when !i + 1 < end_ && s.[!i + 1] = '(' -> (
        match find_block_ref_end s !i with
        | Some e when e <= end_ ->
          let id = String.sub s (!i + 2) (e - !i - 4) in
          acc := (block_ref_link id, None) :: !acc;
          i := e
        | _ -> incr i)
      | _ -> incr i
    done;
    if !complex then
      None
    else
      Some (List.rev !acc)

let try_fast_scan s = try_fast_scan_range s 0 (String.length s)

let parse config =
  take_while (fun _ -> true) >>= fun s ->
  match try_fast_scan s with
  | Some result -> return result
  | None -> (
    match parse_string ~consume:All (parse_angstrom config) s with
    | Ok result -> return result
    | Error e -> fail e)

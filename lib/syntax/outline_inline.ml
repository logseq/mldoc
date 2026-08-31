open! Prelude
open Angstrom

(** Outline mode keeps Plain text plus node refs and tags (properties are
    block-level). Emphasis/code/timestamps are left as Plain. *)
let is_outline_special = function
  | '#'
  | '['
  | '(' ->
    true
  | _ -> false

let may_have_outline_markup _config s =
  let n = String.length s in
  let rec loop i =
    if i >= n then
      false
    else if is_outline_special s.[i] then
      true
    else
      loop (i + 1)
  in
  loop 0

let interesting config : Inline.t Angstrom.t =
  peek_char_fail >>= function
  | '#' -> Inline.hash_tag config
  | '[' -> Inline.nested_link_or_link config
  | '(' -> Inline.block_reference config
  | _ -> fail "not outline inline"

let inline_choices config : Inline.t_with_pos Angstrom.t =
  interesting config
  >>| (fun t -> (t, None))
  <|> ( take_while1 (fun c -> not (is_outline_special c)) >>| fun s ->
        (Inline.Plain s, None) )
  <|> (any_char >>| fun c -> (Inline.Plain (String.make 1 c), None))

let parse_angstrom config =
  many1 (inline_choices config) >>| Inline.concat_plains <?> "outline inline"

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

let find_page_ref_end s i =
  let n = String.length s in
  if i + 1 >= n || s.[i] <> '[' || s.[i + 1] <> '[' then
    None
  else
    let rec loop j depth =
      if j + 1 >= n then
        None
      else if s.[j] = '[' && s.[j + 1] = '[' then
        loop (j + 2) (depth + 1)
      else if s.[j] = ']' && s.[j + 1] = ']' then
        if depth = 1 then
          Some (j + 2)
        else
          loop (j + 2) (depth - 1)
      else
        loop (j + 1) depth
    in
    loop (i + 2) 1

let find_block_ref_end s i =
  let n = String.length s in
  if i + 1 >= n || s.[i] <> '(' || s.[i + 1] <> '(' then
    None
  else
    let rec loop j =
      if j + 1 >= n then
        None
      else if s.[j] = ')' && s.[j + 1] = ')' then
        Some (j + 2)
      else
        loop (j + 1)
    in
    loop (i + 2)

let page_ref_link name =
  Inline.Link
    { url = Inline.Page_ref name
    ; label = [ Inline.Plain "" ]
    ; title = None
    ; full_text = "[[" ^ name ^ "]]"
    ; metadata = ""
    }

let block_ref_link id =
  Inline.Link
    { url = Inline.Block_ref id
    ; label = [ Inline.Plain "" ]
    ; title = None
    ; full_text = "((" ^ id ^ "))"
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

(** Fast path for Plain + #tag / [[page]] / ((block)). Returns None when
    markdown links or nested-page hashtags need the angstrom parser.
    Scans [s] from [off] with length [len] (no need to sub the whole title). *)
let try_fast_scan_range s off len =
  if len < 0 || off < 0 || off + len > String.length s then
    None
  else if
    let rec has_bs i = i < off + len && (s.[i] = '\\' || has_bs (i + 1)) in
    has_bs off
  then
    None
  else
    let end_ = off + len in
    let acc = ref [] in
    let i = ref off in
    let plain_start = ref off in
    let complex = ref false in
    let flush_plain stop =
      if stop > !plain_start then
        acc :=
          Inline.Plain (String.sub s !plain_start (stop - !plain_start)) :: !acc
    in
    while !i < end_ && not !complex do
      match s.[!i] with
      | '#'
        when !i + 1 < end_
             && (not (is_ws s.[!i + 1]))
             && s.[!i + 1] <> '#'
             && (!i = off || is_ws s.[!i - 1]) ->
        flush_plain !i;
        let start = !i + 1 in
        let j = ref start in
        let has_bracket = ref false in
        while !j < end_ && not (is_ws s.[!j]) do
          if s.[!j] = '[' then has_bracket := true;
          incr j
        done;
        if !has_bracket then
          complex := true
        else
          let raw = String.sub s start (!j - start) in
          let name = strip_tag_trail raw in
          if name = "" then
            complex := true
          else (
            acc := Inline.Tag [ Inline.Plain name ] :: !acc;
            let nl = String.length name in
            if nl < String.length raw then
              acc :=
                Inline.Plain (String.sub raw nl (String.length raw - nl))
                :: !acc;
            i := !j;
            plain_start := !j
          )
      | '[' when !i + 1 < end_ && s.[!i + 1] = '[' -> (
        (* find_page_ref_end walks to string end; clamp by checking within range *)
        match find_page_ref_end s !i with
        | Some e when e <= end_ ->
          let name = String.sub s (!i + 2) (e - !i - 4) in
          (* Nested [[…]] needs Nested_link — fall back to Angstrom. *)
          if String.contains name '[' then
            complex := true
          else (
            flush_plain !i;
            acc := page_ref_link name :: !acc;
            i := e;
            plain_start := e
          )
        | _ -> complex := true)
      | '[' -> complex := true
      | '(' when !i + 1 < end_ && s.[!i + 1] = '(' -> (
        match find_block_ref_end s !i with
        | Some e when e <= end_ ->
          flush_plain !i;
          let id = String.sub s (!i + 2) (e - !i - 4) in
          acc := block_ref_link id :: !acc;
          i := e;
          plain_start := e
        | _ -> incr i)
      | _ -> incr i
    done;
    if !complex then
      None
    else (
      flush_plain end_;
      Some
        (Inline.concat_plains
           (Type_op.inline_list_with_none_pos (List.rev !acc)))
    )

let try_fast_scan s = try_fast_scan_range s 0 (String.length s)

let parse config =
  take_while (fun _ -> true) >>= fun s ->
  match try_fast_scan s with
  | Some result -> return result
  | None -> (
    match parse_string ~consume:All (parse_angstrom config) s with
    | Ok result -> return result
    | Error e -> fail e)

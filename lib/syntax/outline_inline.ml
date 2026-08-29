open! Prelude
open Angstrom
open Parsers

(** Outline mode only extracts node refs and tags (properties are block-level). *)
let is_outline_special = function '#' | '[' | '(' -> true | _ -> false

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
    (interesting config >>| fun t -> Some (t, None))
    <|> (any_char *> skip_plain_run *> return None)

let parse_angstrom config =
  many1 (inline_choices config)
  >>| (fun l ->
        let l = List.filter_map (fun x -> x) l in
        Inline.concat_plains l)
  <?> "outline inline"

let is_ws = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let tag_trail = function
  | ',' | ';' | '.' | '!' | '?' | '\'' | '"' | ':' | '#' -> true
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

(** Fast path for #tag / [[page]] / ((block)). Returns None when markdown
    links or nested-page hashtags need the angstrom parser. *)
let try_fast_scan s =
  (* Escapes / backslashes need the real parser. *)
  if String.contains s '\\' then
    None
  else
    let n = String.length s in
    let acc = ref [] in
    let i = ref 0 in
    let complex = ref false in
    while !i < n && not !complex do
      match s.[!i] with
      | '#' when !i + 1 < n && not (is_ws s.[!i + 1]) && s.[!i + 1] <> '#' ->
        let start = !i + 1 in
        let j = ref start in
        let has_bracket = ref false in
        while !j < n && not (is_ws s.[!j]) do
          if s.[!j] = '[' then
            has_bracket := true;
          incr j
        done;
        if !has_bracket then
          complex := true
        else
          let name = strip_tag_trail (String.sub s start (!j - start)) in
          if name <> "" then
            acc := Inline.Tag [ Inline.Plain name ] :: !acc;
          i := !j
      | '[' when !i + 1 < n && s.[!i + 1] = '[' -> (
        match find_page_ref_end s !i with
        | Some e ->
          let name = String.sub s (!i + 2) (e - !i - 4) in
          acc := page_ref_link name :: !acc;
          i := e
        | None -> complex := true)
      | '[' -> complex := true
      | '(' when !i + 1 < n && s.[!i + 1] = '(' -> (
        match find_block_ref_end s !i with
        | Some e ->
          let id = String.sub s (!i + 2) (e - !i - 4) in
          acc := block_ref_link id :: !acc;
          i := e
        | None -> incr i)
      | _ -> incr i
    done;
    if !complex then
      None
    else
      Some (Type_op.inline_list_with_none_pos (List.rev !acc))

let parse config =
  take_while (fun _ -> true) >>= fun s ->
  match try_fast_scan s with
  | Some result -> return result
  | None -> (
    match parse_string ~consume:All (parse_angstrom config) s with
    | Ok result -> return result
    | Error e -> fail e)

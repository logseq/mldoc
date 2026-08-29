(* Fast Markdown outline_only document parser.
   Line-oriented; avoids Angstrom choice/backtracking on the Logseq hot path.
   Extracts headings, properties, lists, quotes, footnotes, and outline inline
   (tags / page refs / block refs / markdown links). *)

open! Prelude
open Type
open Conf

let dummy = Pos.dummy_pos

let with_pos t = (t, dummy)

let markers =
  [| "IN-PROGRESS"
   ; "CANCELLED"
   ; "CANCELED"
   ; "WAITING"
   ; "STARTED"
   ; "DOING"
   ; "TODO"
   ; "WAIT"
   ; "DONE"
   ; "NOW"
   ; "LATER"
  |]

let is_space_char = function
  | ' '
  | '\t' ->
    true
  | _ -> false

let rstrip_cr s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '\r' then
    String.sub s 0 (n - 1)
  else
    s

let skip_spaces s i =
  let n = String.length s in
  let rec loop j =
    if j < n && is_space_char s.[j] then
      loop (j + 1)
    else
      j
  in
  loop i

let indent_len s =
  let n = String.length s in
  let rec loop i =
    if i < n && is_space_char s.[i] then
      loop (i + 1)
    else
      i
  in
  loop 0

let starts_with_at s i prefix =
  let plen = String.length prefix in
  let n = String.length s in
  i + plen <= n
  &&
  let rec loop k =
    if k = plen then
      true
    else if s.[i + k] = prefix.[k] then
      loop (k + 1)
    else
      false
  in
  loop 0

let is_blank_line s =
  let n = String.length s in
  let rec loop i =
    if i >= n then
      true
    else if is_space_char s.[i] then
      loop (i + 1)
    else
      false
  in
  loop 0

let is_fence_line line =
  let ind = indent_len line in
  let n = String.length line in
  ind + 3 <= n
  && line.[ind] = '`'
  && line.[ind + 1] = '`'
  && line.[ind + 2] = '`'

let is_quote_line line =
  let ind = indent_len line in
  ind < String.length line && line.[ind] = '>'

let is_properties_start line =
  String.lowercase_ascii (String.trim line) = ":properties:"

let is_end_mark line = String.lowercase_ascii (String.trim line) = ":end:"

let is_list_item_prefix line =
  let ind = indent_len line in
  let n = String.length line in
  if ind + 2 <= n then
    let c = line.[ind] in
    if (c = '+' || c = '*') && is_space_char line.[ind + 1] then
      true
    else if c >= '0' && c <= '9' then
      let j = ref (ind + 1) in
      while !j < n && line.[!j] >= '0' && line.[!j] <= '9' do
        incr j
      done;
      !j < n && line.[!j] = '.' && !j + 1 < n && is_space_char line.[!j + 1]
    else
      false
  else
    false

let outline_inlines config s =
  if s = "" then
    []
  else if Outline_inline.may_have_outline_markup config s then
    match Outline_inline.try_fast_scan s with
    | Some r -> r
    | None -> (
      match
        Angstrom.parse_string ~consume:All (Outline_inline.parse config) s
      with
      | Ok r -> r
      | Error _ -> [])
  else
    []

let outline_paragraph config s = Paragraph (outline_inlines config s)

let filter_prop_refs inlines =
  List.map fst inlines
  |> List.filter (function
       | Inline.Tag _
       | Inline.Link _
       | Inline.Nested_link _ ->
         true
       | _ -> false)

let heading ~level ~unordered ~size ~marker ~priority ~title =
  Heading
    { level
    ; marker
    ; priority
    ; title
    ; tags = []
    ; anchor = ""
    ; meta = { timestamps = []; properties = [] }
    ; numbering = None
    ; unordered
    ; size
    }

let try_marker s i =
  if i >= String.length s then
    None
  else
    let rec loop k =
      if k >= Array.length markers then
        None
      else
        let m = markers.(k) in
        if starts_with_at s i m then
          let j = i + String.length m in
          if j >= String.length s || is_space_char s.[j] then
            Some (m, j)
          else
            loop (k + 1)
        else
          loop (k + 1)
    in
    loop 0

let try_priority s i =
  let n = String.length s in
  if i + 3 < n && s.[i] = '[' && s.[i + 1] = '#' && s.[i + 3] = ']' then
    Some (s.[i + 2], i + 4)
  else
    None

let parse_marker_priority_title config s i =
  let i = skip_spaces s i in
  let marker, i =
    match try_marker s i with
    | Some (m, j) -> (Some m, skip_spaces s j)
    | None -> (None, i)
  in
  let priority, i =
    match try_priority s i with
    | Some (p, j) -> (Some p, skip_spaces s j)
    | None -> (None, i)
  in
  let title =
    if i >= String.length s then
      ""
    else
      String.sub s i (String.length s - i)
  in
  let title_inlines, keep_title =
    if title = "" then
      ([], true)
    else
      match title.[0] with
      | '`'
      | '>' ->
        ([], false)
      | _ ->
        (outline_inlines config title, true)
  in
  (marker, priority, title_inlines, keep_title, title)

let parse_size_hashes s i =
  let n = String.length s in
  if i >= n || s.[i] <> '#' then
    (None, i)
  else
    let j = ref i in
    while !j < n && s.[!j] = '#' do
      incr j
    done;
    let count = !j - i in
    if !j >= n || is_space_char s.[!j] then
      (Some count, !j)
    else
      (None, i)

(** [Some (heading, opens_fence)] *)
let try_dash_heading config line =
  let ind = indent_len line in
  let n = String.length line in
  if ind >= n || line.[ind] <> '-' then
    None
  else if ind + 1 < n && not (is_space_char line.[ind + 1]) then
    if ind + 1 = n then
      Some
        ( heading ~level:(ind + 1) ~unordered:true ~size:None ~marker:None
            ~priority:None ~title:[]
        , false )
    else
      None
  else
    let i = skip_spaces line (ind + 1) in
    let size, i = parse_size_hashes line i in
    let marker, priority, title, keep_title, raw_title =
      parse_marker_priority_title config line i
    in
    let opens_fence =
      (not keep_title) && String.length raw_title > 0 && raw_title.[0] = '`'
    in
    Some
      ( heading ~level:(ind + 1) ~unordered:true ~size ~marker ~priority ~title
      , opens_fence )

let try_atx_heading config line =
  let ind = indent_len line in
  let n = String.length line in
  if ind >= n || line.[ind] <> '#' then
    None
  else
    let j = ref ind in
    while !j < n && line.[!j] = '#' do
      incr j
    done;
    let size = !j - ind in
    if size = 0 then
      None
    else if !j < n && not (is_space_char line.[!j]) then
      None
    else
      let marker, priority, title, _, _ =
        parse_marker_priority_title config line !j
      in
      Some
        (heading ~level:(ind + 1) ~unordered:false ~size:(Some size) ~marker
           ~priority ~title)

let try_md_property config line =
  let ind = indent_len line in
  let n = String.length line in
  let i = ind in
  if i >= n then
    None
  else
    let key_start = i in
    let j = ref i in
    while
      !j < n
      && line.[!j] <> ':'
      && (not (is_space_char line.[!j]))
      && line.[!j] <> '\n'
    do
      incr j
    done;
    if !j = key_start then
      None
    else if !j + 1 < n && line.[!j] = ':' && line.[!j + 1] = ':' then
      let key = String.sub line key_start (!j - key_start) in
      let rest_i = skip_spaces line (!j + 2) in
      let value =
        if rest_i >= n then
          ""
        else
          String.trim (String.sub line rest_i (n - rest_i))
      in
      Some (key, value, filter_prop_refs (outline_inlines config value))
    else
      None

let try_org_style_prop line =
  let ind = indent_len line in
  let n = String.length line in
  if ind + 2 >= n || line.[ind] <> '#' || line.[ind + 1] <> '+' then
    None
  else
    let i = ind + 2 in
    let j = ref i in
    while !j < n && line.[!j] <> ':' && not (is_space_char line.[!j]) do
      incr j
    done;
    if !j > i && !j < n && line.[!j] = ':' then
      let name = String.sub line i (!j - i) in
      let rest_i = skip_spaces line (!j + 1) in
      let value =
        if rest_i >= n then
          ""
        else
          String.sub line rest_i (n - rest_i)
      in
      Some (name, value, [])
    else
      None

let try_org_drawer_prop_line config line =
  let ind = indent_len line in
  let n = String.length line in
  if ind >= n || line.[ind] <> ':' then
    None
  else
    let i = ind + 1 in
    let j = ref i in
    while !j < n && line.[!j] <> ':' && not (is_space_char line.[!j]) do
      incr j
    done;
    if !j > i && !j < n && line.[!j] = ':' then
      let key = String.sub line i (!j - i) in
      if String.lowercase_ascii key = "end" then
        None
      else
        let rest_i = skip_spaces line (!j + 1) in
        let value =
          if rest_i >= n then
            ""
          else
            String.trim (String.sub line rest_i (n - rest_i))
        in
        Some (key, value, filter_prop_refs (outline_inlines config value))
    else
      None

let try_footnote_line config line =
  let ind = indent_len line in
  let n = String.length line in
  if ind + 3 <= n && line.[ind] = '[' && line.[ind + 1] = '^' then
    try
      let close = String.index_from line (ind + 2) ']' in
      if close + 1 < n && line.[close + 1] = ':' then
        let name = String.sub line (ind + 2) (close - ind - 2) in
        let rest_i = skip_spaces line (close + 2) in
        let body =
          if rest_i >= n then
            ""
          else
            String.sub line rest_i (n - rest_i)
        in
        let inlines =
          if body = "" then
            []
          else if Outline_inline.may_have_outline_markup config body then
            outline_inlines config body
          else
            Type_op.inline_list_with_none_pos [ Inline.Plain body ]
        in
        Some (Footnote_Definition (name, inlines))
      else
        None
    with Not_found -> None
  else
    None

let collect_properties_drawer config lines i =
  if not (is_properties_start lines.(i)) then
    None
  else
    let rec loop j acc =
      if j >= Array.length lines then
        Some (List.rev acc, j)
      else if is_end_mark lines.(j) then
        Some (List.rev acc, j + 1)
      else
        match try_org_drawer_prop_line config lines.(j) with
        | Some kv -> loop (j + 1) (kv :: acc)
        | None ->
          if is_blank_line lines.(j) then
            loop (j + 1) acc
          else
            loop (j + 1) acc
    in
    loop (i + 1) []

let collect_properties config lines i =
  let rec loop j acc =
    if j >= Array.length lines then
      (List.rev acc, j)
    else
      match try_md_property config lines.(j) with
      | Some kv -> loop (j + 1) (kv :: acc)
      | None -> (
        match try_org_style_prop lines.(j) with
        | Some kv -> loop (j + 1) (kv :: acc)
        | None -> (List.rev acc, j))
  in
  loop i []

let is_block_boundary config line =
  is_blank_line line
  || try_dash_heading config line <> None
  || try_atx_heading config line <> None
  || is_fence_line line
  || is_quote_line line
  || is_list_item_prefix line
  || is_properties_start line
  || try_md_property config line <> None
  || try_org_style_prop line <> None
  || try_footnote_line config line <> None

let collect_paragraph_lines config lines i =
  let n = Array.length lines in
  let rec loop j acc =
    if j >= n then
      (List.rev acc, j)
    else if is_block_boundary config lines.(j) then
      (List.rev acc, j)
    else
      loop (j + 1) (lines.(j) :: acc)
  in
  let ls, j = loop i [] in
  let content = String.concat "\n" ls in
  (outline_paragraph config content, j)

let skip_fence_body lines i =
  let rec loop j =
    if j >= Array.length lines then
      j
    else if is_fence_line lines.(j) then
      j + 1
    else
      loop (j + 1)
  in
  loop i

let skip_fence lines i = skip_fence_body lines (i + 1)

let collect_quote config lines i =
  let rec loop j acc =
    if j >= Array.length lines then
      (List.rev acc, j)
    else if is_quote_line lines.(j) then
      let line = lines.(j) in
      let ind = indent_len line in
      let body_i =
        if ind < String.length line && line.[ind] = '>' then
          skip_spaces line (ind + 1)
        else
          ind
      in
      let body =
        if body_i >= String.length line then
          ""
        else
          String.sub line body_i (String.length line - body_i)
      in
      loop (j + 1) (body :: acc)
    else
      (List.rev acc, j)
  in
  let bodies, j = loop i [] in
  (* Match Angstrom quote + concat_paragraph_lines: one merged paragraph. *)
  let content = String.concat "\n" bodies in
  (Quote [ outline_paragraph config content ], j)

let parse_list_item_line line =
  let ind = indent_len line in
  let n = String.length line in
  let c = line.[ind] in
  if c = '+' || c = '*' then
    let content = String.trim (String.sub line (ind + 2) (n - ind - 2)) in
    (ind, false, None, content)
  else
    let j = ref ind in
    while !j < n && line.[!j] >= '0' && line.[!j] <= '9' do
      incr j
    done;
    let num_str = String.sub line ind (!j - ind) in
    let content = String.trim (String.sub line (!j + 2) (n - !j - 2)) in
    (ind, true, Some (int_of_string num_str), content)

let make_list_item config ~indent ~ordered ~number content children =
  { content =
      (if content = "" then
         []
       else
         [ outline_paragraph config content ])
  ; items = children
  ; number
  ; name = []
  ; checkbox = None
  ; indent
  ; ordered
  }

let rec parse_list_items config lines i min_indent =
  let items = ref [] in
  let j = ref i in
  let continue = ref true in
  while !continue && !j < Array.length lines do
    let line = lines.(!j) in
    if is_blank_line line then
      incr j
    else if
      try_dash_heading config line <> None || try_atx_heading config line <> None
    then
      continue := false
    else if is_list_item_prefix line then
      let indent, ordered, number, content = parse_list_item_line line in
      if indent < min_indent then
        continue := false
      else (
        incr j;
        let children, j' =
          if !j < Array.length lines && is_list_item_prefix lines.(!j) then
            let child_indent = indent_len lines.(!j) in
            if child_indent > indent then
              parse_list_items config lines !j child_indent
            else
              ([], !j)
          else
            ([], !j)
        in
        j := j';
        items :=
          make_list_item config ~indent ~ordered ~number content children
          :: !items)
    else
      continue := false
  done;
  (List.rev !items, !j)

let parse config input =
  let raw_lines = String.split_on_char '\n' input in
  let lines = Array.of_list (List.map rstrip_cr raw_lines) in
  let n = Array.length lines in
  let acc = ref [] in
  let i = ref 0 in
  while !i < n do
    let line = lines.(!i) in
    if is_blank_line line then
      incr i
    else
      match try_dash_heading config line with
      | Some (h, opens_fence) ->
        acc := with_pos h :: !acc;
        incr i;
        if opens_fence then
          i := skip_fence_body lines !i
      | None -> (
        match try_atx_heading config line with
        | Some h ->
          acc := with_pos h :: !acc;
          incr i
        | None -> (
          match try_footnote_line config line with
          | Some fn ->
            acc := with_pos fn :: !acc;
            incr i
          | None -> (
            match collect_properties_drawer config lines !i with
            | Some (kvs, j) ->
              acc := with_pos (Property_Drawer kvs) :: !acc;
              i := j
            | None -> (
              match collect_properties config lines !i with
              | _ :: _ as kvs, j ->
                acc := with_pos (Property_Drawer kvs) :: !acc;
                i := j
              | [], _ ->
                if is_fence_line line then
                  i := skip_fence lines !i
                else if is_quote_line line then
                  let q, j = collect_quote config lines !i in
                  acc := with_pos q :: !acc;
                  i := j
                else if is_list_item_prefix line then
                  let items, j =
                    parse_list_items config lines !i (indent_len line)
                  in
                  acc := with_pos (List items) :: !acc;
                  i := j
                else
                  let p, j = collect_paragraph_lines config lines !i in
                  acc := with_pos p :: !acc;
                  i := j))))
  done;
  List.rev !acc

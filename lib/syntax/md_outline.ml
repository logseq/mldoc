(* Fast Markdown document parser (outline_only + full).
   Line-oriented; avoids Angstrom choice/backtracking on the Logseq hot path.
   Outline: headings, properties, lists, quotes, footnotes, outline inline.
   Full: same structure with Inline.parse, Src fences, latex env, anchors. *)

open! Prelude
open Type
open Conf

let plain_inlines s = Type_op.inline_list_with_none_pos [ Inline.Plain s ]

let ensure_trailing_nl s =
  let n = String.length s in
  if n = 0 || s.[n - 1] = '\n' then
    s
  else
    s ^ "\n"

let separate_name_options = function
  | None
  | Some "" ->
    (None, None)
  | Some s -> (
    match String.split_on_char ' ' (String.trim s) with
    | [] -> (None, None)
    | [ name ] -> (Some name, None)
    | name :: options -> (Some name, Some options))

let anchor_of_title title =
  Type_parser.Heading.anchor_link
    (Inline.asciis (Type_op.inline_list_strip_pos title))

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
    else if c >= '0' && c <= '9' then (
      let j = ref (ind + 1) in
      while !j < n && line.[!j] >= '0' && line.[!j] <= '9' do
        incr j
      done;
      !j < n && line.[!j] = '.' && !j + 1 < n && is_space_char line.[!j + 1]
    ) else
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
      | Error _ -> plain_inlines s)
  else
    plain_inlines s

let full_inlines config s =
  if s = "" then
    []
  else
    match Angstrom.parse_string ~consume:All (Inline.parse config) s with
    | Ok r -> r
    | Error _ -> []

let content_inlines config s =
  if config.parse_outline_only then
    outline_inlines config s
  else
    full_inlines config s

let content_paragraph config s = Paragraph (content_inlines config s)

(** Quotes: Angstrom records eol after each line → trailing Break_Line. *)
let quote_paragraph config s =
  let s =
    if config.parse_outline_only then
      s
    else
      ensure_trailing_nl s
  in
  Paragraph (content_inlines config s)

let heading ~outline_only ~level ~unordered ~size ~marker ~priority ~title =
  Heading
    { level
    ; marker
    ; priority
    ; title
    ; tags = []
    ; anchor =
        (if outline_only then
           ""
         else
           anchor_of_title title)
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
      | '>' ->
        (* Leave markdown quote for the following block (Angstrom parity). *)
        ([], false)
      | '`'
      | '~'
        when String.length title >= 3
             && title.[1] = title.[0]
             && title.[2] = title.[0] ->
        (* Fenced code opener on the heading line. *)
        ([], false)
      | _ -> (content_inlines config title, true)
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

(** [Some (heading, rest)] where [rest] is a fence header or quote line left
    on the same source line after the heading marker. *)
type heading_rest =
  | Nothing
  | Fence of string
  | Quote_line of string

let try_dash_heading config line =
  let ind = indent_len line in
  let n = String.length line in
  if ind >= n || line.[ind] <> '-' then
    None
  else if ind + 1 < n && not (is_space_char line.[ind + 1]) then
    if ind + 1 = n then
      Some
        ( heading ~outline_only:config.parse_outline_only ~level:(ind + 1)
            ~unordered:true ~size:None ~marker:None ~priority:None ~title:[]
        , Nothing )
    else
      None
  else
    let i = skip_spaces line (ind + 1) in
    let size, i = parse_size_hashes line i in
    let marker, priority, title, keep_title, raw_title =
      parse_marker_priority_title config line i
    in
    let rest =
      if keep_title || raw_title = "" then
        Nothing
      else if raw_title.[0] = '>' then
        Quote_line raw_title
      else if
        (raw_title.[0] = '`' || raw_title.[0] = '~')
        && String.length raw_title >= 3
        && raw_title.[1] = raw_title.[0]
        && raw_title.[2] = raw_title.[0]
      then
        Fence raw_title
      else
        Nothing
    in
    Some
      ( heading ~outline_only:config.parse_outline_only ~level:(ind + 1)
          ~unordered:true ~size ~marker ~priority ~title
      , rest )

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
        (heading ~outline_only:config.parse_outline_only ~level:(ind + 1)
           ~unordered:false ~size:(Some size) ~marker ~priority ~title)

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
      Some (key, value, Property.property_references config value)
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
        Some (key, value, Property.property_references config value)
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
          else if config.parse_outline_only then
            outline_inlines config body
          else
            content_inlines config body
        in
        Some (Footnote_Definition (name, inlines))
      else
        None
    with
    | Not_found -> None
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
  || is_fence_line line || is_quote_line line || is_list_item_prefix line
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
  (* When another block follows, each line was newline-terminated in the
     source — Angstrom records a final Break_Line. *)
  let content =
    if (not config.parse_outline_only) && j < n && content <> "" then
      ensure_trailing_nl content
    else
      content
  in
  (content_paragraph config content, j)

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

(** Collect fenced Src given an opening fence header (e.g. "```ocaml") and
    body lines starting at index [i]. [body_start_pos] / [body_end_pos] are
    byte offsets in the original input (Angstrom pos_meta parity). *)
let collect_src_from_header ~body_start_pos ~body_end_pos lines i fence_header =
  let rest =
    if String.length fence_header >= 3 then
      String.trim (String.sub fence_header 3 (String.length fence_header - 3))
    else
      ""
  in
  let language, options = separate_name_options (Some rest) in
  let rec loop j acc =
    if j >= Array.length lines then
      (List.rev acc, j)
    else if is_fence_line lines.(j) then
      (List.rev acc, j + 1)
    else
      loop (j + 1) ("\n" :: lines.(j) :: acc)
  in
  let body_lines, j = loop i [] in
  ( Src
      { lines = body_lines
      ; language
      ; options
      ; pos_meta = { Pos.start_pos = body_start_pos; end_pos = body_end_pos }
      }
  , j )

let collect_src ~line_starts lines i =
  let open_line = lines.(i) in
  let ind = indent_len open_line in
  let fence_header = String.sub open_line ind (String.length open_line - ind) in
  let body_i = i + 1 in
  let body_start_pos =
    if body_i < Array.length lines then
      line_starts.(body_i)
    else
      line_starts.(i) + String.length open_line + 1
  in
  (* Find closing fence to compute end_pos = start of closing fence line. *)
  let rec find_end j =
    if j >= Array.length lines then
      if Array.length lines = 0 then
        body_start_pos
      else
        line_starts.(Array.length lines - 1)
        + String.length lines.(Array.length lines - 1)
    else if is_fence_line lines.(j) then
      line_starts.(j)
    else
      find_end (j + 1)
  in
  let body_end_pos = find_end body_i in
  collect_src_from_header ~body_start_pos ~body_end_pos lines body_i
    fence_header

let quote_continuation_stop config line =
  (* Match Block.md_blockquote: stop only on new block markers. *)
  let trimmed = String.trim line in
  try_dash_heading config line <> None
  || try_atx_heading config line <> None
  || is_list_item_prefix line || is_fence_line line || is_properties_start line
  || starts_with_at line 0 "- " || starts_with_at line 0 "# "
  || starts_with_at line 0 "id:: "
  || trimmed = "-" || trimmed = "#"

let collect_quote config ?first_line lines i =
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
    else if
      (not config.parse_outline_only)
      && not (quote_continuation_stop config lines.(j))
    then
      (* Full MD: lines without '>' still belong to the quote. *)
      loop (j + 1) (lines.(j) :: acc)
    else
      (List.rev acc, j)
  in
  let start_acc, start_j =
    match first_line with
    | None -> ([], i)
    | Some line ->
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
      ([ body ], i)
  in
  let bodies, j = loop start_j start_acc in
  (* Match Angstrom quote + concat_paragraph_lines: one merged paragraph. *)
  let content = String.concat "\n" bodies in
  (Quote [ quote_paragraph config content ], j)

let try_latex_environment line =
  let s = String.trim line in
  let n = String.length s in
  if n < 8 || not (starts_with_at s 0 "\\begin{") then
    None
  else
    let name_start = 7 in
    match String.index_from_opt s name_start '}' with
    | None -> None
    | Some name_end -> (
      let name = String.sub s name_start (name_end - name_start) in
      let ending = "\\end{" ^ name ^ "}" in
      let ending_l = String.lowercase_ascii ending in
      let rec find_end i =
        if i + String.length ending > n then
          None
        else if
          String.lowercase_ascii (String.sub s i (String.length ending))
          = ending_l
        then
          Some i
        else
          find_end (i + 1)
      in
      match find_end (name_end + 1) with
      | None -> None
      | Some end_i ->
        let content = String.sub s (name_end + 1) (end_i - name_end - 1) in
        Some (Latex_Environment (String.lowercase_ascii name, None, content)))

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
         [ content_paragraph config content ])
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
      try_dash_heading config line <> None
      || try_atx_heading config line <> None
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
          :: !items
      )
    else
      continue := false
  done;
  (List.rev !items, !j)

let parse config input =
  let raw_split = String.split_on_char '\n' input in
  let decoded =
    List.map
      (fun s ->
        let n = String.length s in
        if n > 0 && s.[n - 1] = '\r' then
          (String.sub s 0 (n - 1), n)
        else
          (s, n))
      raw_split
  in
  let lines = Array.of_list (List.map fst decoded) in
  let raw_lens = Array.of_list (List.map snd decoded) in
  let n = Array.length lines in
  let input_len = String.length input in
  let line_starts =
    let arr = Array.make (max n 1) 0 in
    let pos = ref 0 in
    for idx = 0 to n - 1 do
      arr.(idx) <- !pos;
      let nl =
        if idx + 1 < n then
          1
        else
          0
      in
      pos := !pos + raw_lens.(idx) + nl
    done;
    arr
  in
  let pos_range i j =
    let start_pos =
      if i < n then
        line_starts.(i)
      else
        input_len
    in
    let end_pos =
      if j < n then
        line_starts.(j)
      else
        input_len
    in
    { Pos.start_pos; end_pos }
  in
  let with_range i j t = (t, pos_range i j) in
  let src_end_pos body_i =
    let rec find j =
      if j >= n then
        input_len
      else if is_fence_line lines.(j) then
        line_starts.(j)
      else
        find (j + 1)
    in
    find body_i
  in
  let acc = ref [] in
  let i = ref 0 in
  while !i < n do
    let line = lines.(!i) in
    if is_blank_line line then
      incr i
    else
      match try_dash_heading config line with
      | Some (h, rest) -> (
        let h_i = !i in
        acc := with_range h_i (h_i + 1) h :: !acc;
        incr i;
        match rest with
        | Nothing -> ()
        | Fence hdr ->
          if config.parse_outline_only then
            i := skip_fence_body lines !i
          else
            let body_i = !i in
            let body_start_pos =
              if body_i < n then
                line_starts.(body_i)
              else
                input_len
            in
            let body_end_pos = src_end_pos body_i in
            let src, j =
              collect_src_from_header ~body_start_pos ~body_end_pos lines body_i
                hdr
            in
            acc := with_range h_i j src :: !acc;
            i := j
        | Quote_line qline ->
          let q_i = h_i in
          let q, j = collect_quote config ~first_line:qline lines !i in
          acc := with_range q_i j q :: !acc;
          i := j)
      | None -> (
        match try_atx_heading config line with
        | Some h ->
          acc := with_range !i (!i + 1) h :: !acc;
          incr i
        | None -> (
          match try_footnote_line config line with
          | Some fn ->
            acc := with_range !i (!i + 1) fn :: !acc;
            incr i
          | None -> (
            match collect_properties_drawer config lines !i with
            | Some (kvs, j) ->
              acc := with_range !i j (Property_Drawer kvs) :: !acc;
              i := j
            | None -> (
              match collect_properties config lines !i with
              | (_ :: _ as kvs), j ->
                acc := with_range !i j (Property_Drawer kvs) :: !acc;
                i := j
              | [], _ -> (
                if is_fence_line line then (
                  if config.parse_outline_only then
                    i := skip_fence lines !i
                  else
                    let start_i = !i in
                    let src, j = collect_src ~line_starts lines !i in
                    acc := with_range start_i j src :: !acc;
                    i := j
                ) else if is_quote_line line then (
                  let start_i = !i in
                  let q, j = collect_quote config lines !i in
                  acc := with_range start_i j q :: !acc;
                  i := j
                ) else if is_list_item_prefix line then (
                  let start_i = !i in
                  let items, j =
                    parse_list_items config lines !i (indent_len line)
                  in
                  acc := with_range start_i j (List items) :: !acc;
                  i := j
                ) else
                  match
                    if config.parse_outline_only then
                      None
                    else
                      try_latex_environment line
                  with
                  | Some latex ->
                    acc := with_range !i (!i + 1) latex :: !acc;
                    incr i
                  | None ->
                    let start_i = !i in
                    let p, j = collect_paragraph_lines config lines !i in
                    acc := with_range start_i j p :: !acc;
                    i := j)))))
  done;
  List.rev !acc

open Prelude
open Angstrom
open Parsers

let match_tag tag open_tag close_tag =
  let level_ref = ref (1) in
  let s_ref = ref "" in
  (* To determine whether the `]` is inside a string. *)
  let double_quotes = ref 0 in
  fix (fun parse ->
      end_string_2 close_tag ~ci:true (fun s ->
          (* FIXME: should exclude `[:` which insides any string *)
          let level = count_substring s open_tag in
          let s' = Str.global_replace (Str.regexp_string "\\\"") "" s in
          let quotes = count_substring s' "\"" in
          let quotes' = !double_quotes + quotes in
          let _ = double_quotes := quotes' in
          let is_even i = i mod 2 = 0 in
          let decrement = if is_even quotes' then 1 else 0 in
          let _ = level_ref := !level_ref + level - decrement in
          let _ = s_ref := !s_ref ^ s ^ close_tag in
          (* let _ = Printf.printf "Hiccup tag level: %d, tag: %s, content: %s, all content so far: %s, level ref: %d, decrement level: %d\n" level tag s !s_ref !level_ref decrement in *)
          if !level_ref <= 0 then
            return (open_tag ^ tag ^ !s_ref)
          else
            parse))

let element =
  string "[:" *> take_till1 (fun c -> is_space c || c == ']')
  >>= fun tag ->
  (if Raw_html.known_tag tag then
     return tag
   else
     fail ("html invalid tag: " ^ tag))
  >>= fun tag ->
  match_tag tag "[:" "]"

let parse =
  peek_string 2 >>= fun s ->
  match s with
  | "[:" ->
    element
  | _ ->
    fail "hiccup"

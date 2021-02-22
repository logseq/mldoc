(* Syntax from https://roamresearch.com *)

(* [[Introduction to [[Logseq]]]] *)

open Prelude
open Angstrom
open Parsers

type child =
    Label of string
  | Nested_link of t
and children = child list
and t = {
  content: string;
  children: children;
} [@@deriving yojson]

let open_brackets = "[["

let close_brackets = "]]"

let match_brackets () =
  let level_ref = ref (1) in
  let s_ref = ref "" in
  (* To determine whether the `]` is inside a string. *)
  string "[[" *>
  fix (fun parse ->
      end_string_2 close_brackets ~ci:true (fun s ->
          let level = count_substring s open_brackets in
          let _ = level_ref := !level_ref + level - 1 in
          let _ = s_ref := !s_ref ^ s ^ close_brackets in
          if !level_ref <= 0 then
            let v = !s_ref in
            let _ = level_ref := 1 in
            let _ = s_ref := "" in
            return (open_brackets ^ v)
          else
            parse))

let label_parse =
  take_while1 (fun c -> c <> '[') >>| fun s -> Label s

let parse =
  fix (fun p ->
      let children_parse = many1 (choice [label_parse; p]) in
      match_brackets ()
      >>= fun s ->
      let inner_s = String.sub s 2 (String.length s - 4) in
      let result = match parse_string ~consume:All children_parse inner_s with
        | Ok result -> result
        | Error _e -> [Label inner_s] in
      return @@ Nested_link {content = s; children = result}) >>= function
  | Label _l -> fail "nested link"
  | Nested_link {content; children} ->
    if List.length children <= 1 then
      fail "nested link"
    else
      return @@ {content; children}

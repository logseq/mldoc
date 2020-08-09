(* From Roam Research [[Introduction to [[Logseq]]]] *)

open Prelude
open Angstrom
open Parsers

let parse =
  let level_ref = ref (1) in
  let s_ref = ref "" in
  fix (fun parse ->
      end_string_2 "]]" (fun s ->
          let level = count_substring s "[[" in
          let _ = level_ref := !level_ref + level - 1 in
          let _ = s_ref := !s_ref ^ s ^ "]]" in
          if !level_ref <= 0 then
            return (!s_ref)
          else
            parse))

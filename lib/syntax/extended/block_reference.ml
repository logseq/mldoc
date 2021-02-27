(* Inspired by https://roamresearch.com *)

(* ((block reference)) *)

open Angstrom
open Parsers

let parse =
  between_string "((" "))"
    (take_while1 (function
      | ')' -> false
      | _ -> true))

open Angstrom
open Parsers

let tag_delims =
  [ '['
  ; ']'
  ; '('
  ; ')'
  ; '{'
  ; '}'
  ; '<'
  ; '>'
  ; '+'
  ; '#'
  ; ','
  ; ';'
  ; '.'
  ; '!'
  ; '?'
  ; '='
  ; '%'
  ]

(* TODO: how to express begin of line? Otherwise the tag should be prefixed by spaces *)
let parse =
  char '#'
  *> take_while1 (fun c -> non_space_eol c && not (List.mem c tag_delims))

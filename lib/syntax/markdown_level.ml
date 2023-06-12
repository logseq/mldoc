open Angstrom
open Parsers

let parse = optional tabs_or_ws >>= fun indents ->
  take_while1 (fun c -> c = '#') >>| fun level_str ->
  (indents, level_str)

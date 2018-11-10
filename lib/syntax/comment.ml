open Angstrom
open Parsers
open Type

let parse =
  let p = string "# " *> line
  >>= fun line ->
  return [Comment line]
  in between_eols_or_spaces p

open Angstrom
open Parsers
open Org

let parse =
  let p = string "# " *> line
  >>= fun line ->
  return [Comment line]
  in between_eols_or_spaces p

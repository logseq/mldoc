open Angstrom
open Parsers
open Org

let parse =
  optional eols *>
  string "# " *> line
  >>= fun line ->
  return [Comment line]

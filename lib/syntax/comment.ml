open Angstrom
open Parsers
open Types

let parse =
  optional eols *>
  string "# " *> line
  >>= fun line ->
  return [Comment line]

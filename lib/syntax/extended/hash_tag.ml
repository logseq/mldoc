open Angstrom
open Parsers

let parse =
  spaces *> char '#' *> non_spaces

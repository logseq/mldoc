open Angstrom
open Parsers
open Type

let org = count 5 (char '-')
let markdown = count 3 (char '-')

let parse =
  let p = choice [org; markdown]
    >>= fun _ -> return [Horizontal_Rule] in
  optional eols *> optional spaces *> p <* (choice [end_of_line; end_of_input])

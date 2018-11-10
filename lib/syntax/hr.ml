open Angstrom
open Parsers
open Type

let parse =
  let p = count 5 (char '-')
    >>= fun _ -> return [Horizontal_Rule] in
  between_eols p

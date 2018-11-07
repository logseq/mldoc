open Angstrom
open Parsers
open Org

let parse =
  let p = count 5 (char '-')
    >>= fun _ -> return [Horizontal_Rule] in
  between_eols_or_spaces p

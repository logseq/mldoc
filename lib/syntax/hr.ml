open Angstrom
open Parsers
open Org

let parse = optional eols *> spaces *> count 5 (char '-')
    >>= fun _ -> return [Horizontal_Rule]

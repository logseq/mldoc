open Angstrom
open Parsers
open Types

let parse = optional eols *> optional ws *> count 5 (char '-')
    >>= fun _ -> return [Horizontal_Rule]

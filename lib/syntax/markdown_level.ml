open Angstrom
open Parsers

let parse = optional ws *> take_while1 (fun c -> c = '#')

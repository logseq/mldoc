open Angstrom
let parse = take_while1 (fun c -> c = '#')

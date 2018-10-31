open Angstrom
open Org

let parse = count 5 (char '-')
    >>= fun _ -> return Horizontal_Rule

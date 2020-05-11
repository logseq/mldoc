open Angstrom
open Parsers

(* End a line with two or more spaces*)
let parse =
  ws <* end_of_line
  >>= fun s ->
  if String.length s >= 2 then
    return s
  else
    fail "At least two spaces"

open Angstrom

let parse input =
  match parse_string Heading.parse input with
  | Ok result -> result
  | Error err -> failwith err

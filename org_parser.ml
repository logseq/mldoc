open Angstrom

let parse input =
  match parse_string Heading.level input with
  | Ok l -> print_int @@ List.length l
  | Error err -> failwith err

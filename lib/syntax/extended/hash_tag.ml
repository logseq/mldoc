open Angstrom
open Parsers

(* TODO: how to express begin of line? Otherwise the tag should be prefixed by spaces *)
let parse =
  char '#' *> take_while1 (fun c ->
      non_space_eol c && c <> '[' && c <> ']' && c <> '(' && c <> ')' && c <> '+' && c <> '#' && c <> ','
    ) <* ((ws >>= fun _ -> return ()) <|> end_of_line <|> end_of_input)

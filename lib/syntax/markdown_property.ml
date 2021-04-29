open Angstrom
open Parsers
open Type

(*
   Add Property syntax to markdown format.
   key::value
 *)

let property =
  let key =
    spaces *> take_while1 (fun c -> c <> ':' && non_space_eol c) <* string "::"
  in
  let value =
    spaces *> take_till is_eol <* (eol *> return () <|> end_of_input)
  in
  both key value

let parse = many1 property >>| fun kvs -> Property_Drawer kvs

open Angstrom
open Parsers
open Type

(*
   Add Property syntax to markdown format.
   key::value
 *)

let property =
  let only_key =
    spaces *> take_while1 (fun c -> c <> ':' && non_space_eol c)
    <* string "::" <* spaces
    <* (eol *> return () <|> end_of_input)
  in
  let key =
    spaces *> take_while1 (fun c -> c <> ':' && non_space_eol c) <* string ":: "
  in
  let value =
    spaces *> take_till is_eol <* (eol *> return () <|> end_of_input)
  in
  lift2 (fun k v -> (k, String.trim v)) key value
  <|> (only_key >>| fun k -> (k, ""))

let parse = many1 property >>| fun kvs -> Property_Drawer kvs

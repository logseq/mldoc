open Angstrom
open Parsers
open Type
open Conf

(*
   Add Property syntax to markdown format.
   key::value
 *)

let property config =
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
  lift2 (fun k v ->
      let value = String.trim v in
      let references = Property.property_references config value in
      (k, value, references)) key value
  <|> (only_key >>| fun k -> (k, "", []))

let parse config = many1 (property config) >>| fun kvs -> Property_Drawer kvs

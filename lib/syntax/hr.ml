open Angstrom
open Parsers
open Type
open Prelude
open Conf

let org = count 5 (char '-')

(* markdown, excuse me...
 ***
   ---
   _________________
*)

let parse config =
  let p =
    let parser = match config.format with
      | Org -> org
      | Markdown -> Markdown_hr.parse
    in
    parser >>= fun s ->
    if List.length s >= 3 && (List.length (remove_dups s)) == 1 then
      return [Horizontal_Rule]
    else
      fail "At least 3 chars"
      >>= fun _ -> return [Horizontal_Rule] in
  optional eols *> optional spaces
  *> p <*
  optional spaces <* (choice [end_of_line; end_of_input])

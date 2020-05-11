open Angstrom
open Parsers
open Type

let org = count 5 (char '-')

(* markdown, excuse me...
 ***
   ---
   _________________
*)

let markdown_char = choice [char '-'; char '*'; char '_']

let rec remove_dups lst =
  match lst with
  | [] -> []
  | h::t -> h::(remove_dups (List.filter (fun x -> x<>h) t))

let parse =
  let p = choice [many1 markdown_char; org]
    >>= fun s ->
    if List.length s >= 3 && (List.length (remove_dups s)) == 1 then
      return [Horizontal_Rule]
    else
      fail "At least 3 chars"
      >>= fun _ -> return [Horizontal_Rule] in
  optional eols *> optional spaces
  *> p <*
  optional spaces <* (choice [end_of_line; end_of_input])

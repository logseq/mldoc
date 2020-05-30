open Angstrom
open Parsers
open Type

(* First Term
 * : This is the definition of the first term.
 *
 * Second Term
 * : This is one definition of the second term.
 * : This is another definition of the second term. *)

(* copy from verbatim *)
let definition_content lines =
  fix (fun definition_content ->
      (spaces *> char ':' <* (optional spaces)) *> take_till is_eol <* optional eol
      >>= fun line ->
      lines := line :: !lines;
      definition_content
      <|>
      (if !lines = [] then
         fail "definition_content"
       else
         return (List.rev !lines)))

let definition_parse config =
  let name = spaces *> line <* eol in
  name >>= fun name ->
  definition_content (ref []) >>= fun lines ->
  let name = match parse_string (Inline.parse config) name with
    | Ok inlines -> inlines
    | Error _e -> [Inline.Plain name] in
  let content = List.map (fun line ->
      match parse_string (Inline.parse config) (String.trim line) with
      | Ok content -> Paragraph content
      | Error _e -> Paragraph [Inline.Plain line]
    ) lines in
  let list = {
    content;
    items = [];
    number = None;
    name;
    checkbox = None;
    indent = 0;                 (* TODO: *)
    ordered = false;
  } in
  return list

let parse config =
  many1 (definition_parse config <* optional eols)

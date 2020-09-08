open Angstrom
open Parsers
open Type

(* First Term
 * : This is the definition of the first term.
 *
 * Second Term
 * : This is one definition of the second term.
 * : This is another definition of the second term. *)

let term_definition =
  let non_colon_or_eol = function
      ':' | '\r' | '\n' | '#' -> false
    | _ -> true in
  let l = spaces *> satisfy non_colon_or_eol >>=
    fun c ->
    line <* (end_of_input <|> end_of_line)
    >>| fun s ->
    Char.escaped c ^ s
  in
  many1 l

let definition_content_item =
  spaces *> char ':' *> term_definition
  >>| function lines ->
    String.concat "\n" lines

(* copy from verbatim *)
let definition_content =
  many1 definition_content_item

let definition_parse config =
  let name = spaces *> line <* eol in
  name >>= fun name ->
  definition_content >>= fun lines ->
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
  let p = Helper.with_pos_meta (definition_parse config <* optional eols) in
  many1 p

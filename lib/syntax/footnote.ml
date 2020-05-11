open Angstrom
open Parsers
open Type
open Conf

(* https://orgmode.org/manual/Footnotes.html *)
(* It ends at the next footnote definition, headline, or after two consecutive empty lines. *)
let footnote_definition lines =
  fix (fun footnote_definition ->
      line <* optional eol >>= fun line ->
      lines := line :: !lines;
      two_eols (List.rev !lines)
      <|>
      (peek_string 4 >>= function
        | "[fn:" ->
          return (List.rev !lines)
        | s when s.[0] = '*' && (List.for_all (fun c -> c = '*' || is_space c) (Prelude.explode s)) ->
          return (List.rev !lines)
        | _ ->
          footnote_definition)
      <|>
      return (List.rev !lines))

let footnote_reference config lines =
  let name_part =
    string "[fn:" *> take_while1 (fun c -> c <> ']' && non_eol c)
    <* char ']' <* optional spaces in
  let definition_part = footnote_definition lines in
  lift2
    (fun name definition ->
       let definition = Inline.footnote_inline_definition config ~break:true (String.concat "\n" definition) in
       Footnote_Definition (name, definition))
    name_part definition_part

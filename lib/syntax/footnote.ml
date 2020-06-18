open Angstrom
open Parsers
open Type
open Conf

(* FIXME: markdown footnote not working well with indent *)

(*
[fn:myfootnote] Extensively used in large documents.

[fn:2] Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit
esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat
non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

great I like it very much
*)

(* https://www.gnu.org/software/emacs/manual/html_node/org/Footnotes.html *)
(* Org mode extends the number-based syntax to named footnotes and optional inline definition. Here are the valid references:
 * 1. [fn:name]
 * A named footnote reference, where name is a unique label word, or, for simplicity of automatic creation, a number.
 * 2. [fn::This is the inline definition of this footnote]
 * A LaTeX-like anonymous footnote where the definition is given directly at the reference point.
 * 3. [fn:name:a definition]
 * An inline definition of a footnote, which also specifies a name for the note. Since Org allows multiple references to the same note, you can then use [fn:name] to create additional references. *)

(* https://orgmode.org/manual/Footnotes.html *)
(* It ends at the next footnote definition, headline, or after two consecutive empty lines. *)
let name_part config = match config.format with
  | "Org" ->
    string "[fn:" *> take_while1 (fun c -> c <> ']' && non_eol c)
    <* char ']' <* spaces
  | "Markdown" ->
    Markdown_footnote.reference <* char ':' <* spaces

let footnote_definition =
  let non_eol = function
      '\r' | '\n' | '*' | '#' -> false
    | _ -> true in
  let l = spaces *> satisfy non_eol >>=
    fun c ->
    line <* (end_of_input <|> end_of_line)
    >>| fun s ->
    Char.escaped c ^ s
  in
  many1 l

let definition_parse config =
  let name_part = name_part config in
  lift2
    (fun name lines ->
       let definition_content = String.concat "\n" lines in
       let definition = match parse_string (Inline.parse config) definition_content with
         | Ok inlines -> inlines
         | Error _e -> [Inline.Plain definition_content] in
       Footnote_Definition (name, definition))
    name_part footnote_definition

let parse config =
  many1 (definition_parse config <* (optional eols))

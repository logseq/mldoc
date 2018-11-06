open Angstrom
open Parsers
open Prelude
open Org

(* There are 2 kinds of blocks.
   1. `begin ... end`
   #+BEGIN_X
   line1
   line 2
   #+END_x

   2. Verbatim, each line starts with `:`.
*)

(* TODO:
   1. Code block supports reference (name attribute).
*)

let verbatim lines =
  fix (fun verbatim ->
      spaces *> char ':' *> ws *> take_till is_eol <* optional eol
      >>= fun line ->
      lines := line :: !lines;
      verbatim
      <|>
      (if !lines = [] then
         fail "verbatim"
       else
         return (List.rev !lines)))

let block_name_options_parser =
  lift2 (fun name options ->
      match options with
      | None | Some "" -> (name, None)
      | _ -> (name, options))
    (string_ci "#+begin_" *> non_spaces)
    (spaces *> optional line)
  <* (optional eol)

(* TODO: abstract, sidebar *)
let parse =
  spaces *> peek_char_fail
  >>= function
  | '#' ->
    block_name_options_parser
    >>= fun (name, options) ->
    print_endline name;
    between_lines (fun line ->
        let prefix = "#+end_" ^ name in
        starts_with line prefix) "block"
    >>| fun lines ->
    let name = String.lowercase_ascii name in
    (match name with
     | "src" -> [Src {lines; language = options}]
     | "quote" -> [Quote lines]
     | "example" -> [Example lines]
     | _ -> [Custom (name, options, lines)])
  | ':' ->                      (* verbatim block *)
    verbatim (ref []) >>|
    fun lines -> [Example lines]
  | _ -> fail "block"

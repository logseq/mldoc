open Angstrom
open Parsers
open Prelude
open Type

(* There are 2 kinds of blocks.
   1. `begin ... end`
   #+BEGIN_X
   line1
   line 2
   #+END_x

   2. Verbatim, each line starts with `:`.
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

let list_content_parsers block_parse =
  many1 (choice [
      Table.parse
    ; block_parse
    ; Directive.parse
    ; Drawer.parse
    ; Latex_env.parse
    ; Hr.parse
    ; Comment.parse
    ; Paragraph.parse [ Table.parse
                      ; block_parse
                      ; Directive.parse
                      ; Drawer.parse
                      ; Latex_env.parse
                      ; Hr.parse
                      ; Comment.parse]
    ])

let block_content_parsers block_parse =
  many1 (choice [
      Table.parse
    ; Lists.parse (list_content_parsers block_parse)
    ; block_parse
    ; Directive.parse
    ; Drawer.parse
    ; Latex_env.parse
    ; Hr.parse
    ; Comment.parse
    ; Paragraph.parse [ Table.parse
                      ; Lists.parse (list_content_parsers block_parse)
                      ; block_parse
                      ; Directive.parse
                      ; Drawer.parse
                      ; Latex_env.parse
                      ; Hr.parse
                      ; Comment.parse]
    ])

let separate_name_options = function
  | None -> (None, None)
  | Some s ->
    match String.split_on_char ' ' s with
    | [] -> (None, None)
    | name :: [] -> (Some name, None)
    | name :: options -> (Some name, Some options)

let rec parse = fix (fun parse ->
    let p = peek_char_fail
      >>= function
      | '#' ->
        block_name_options_parser
        >>= fun (name, options) ->
        between_lines ~trim:false (fun line ->
            let prefix = "#+end_" ^ name in
            starts_with (String.trim line) prefix) "block"
        >>| fun lines ->
        (* clear indents *)
        let indent = get_indent (List.hd lines) in
        let lines = if indent = 0 then lines else
            List.map (fun line ->
                String.sub line indent (String.length line - indent)) lines in
        let name = String.lowercase_ascii name in
        (match name with
         | "src" ->
           let (language, options) = separate_name_options options in
           [Src {language; options; lines}]
         | "example" -> [Example lines]
         | "quote" ->
           let content = String.concat "\n" lines in
           let result = match parse_string (block_content_parsers parse) content with
             | Ok result -> result
             | Error e -> [] in
           [Quote (List.concat result)]
         | "export" ->          (* export html, etc *)
           let (name, options) = separate_name_options options in
           let name = match name with None -> "" | Some s -> s in
           let content = String.concat "\n" lines in
           [Export (name, options, content)]
         | _ ->
           let content = String.concat "\n" lines in
           let result = match parse_string (block_content_parsers parse) content with
             | Ok result -> result
             | Error e -> [] in
           [Custom (name, options, List.concat result)]
        )
      | ':' ->                      (* verbatim block *)
        verbatim (ref []) >>|
        fun lines -> [Example lines]
      | _ -> fail "block" in
    between_eols p)

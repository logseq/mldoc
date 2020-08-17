open Angstrom
open Parsers
open Prelude
open Type
open Conf

(* There are 2 kinds of blocks.
   1. `begin ... end`
   #+BEGIN_X
   line1
   line 2
   #+END_x

   2. Verbatim, each line starts with `:`.
*)

let results =
  spaces *> string "#+RESULTS:" >>= fun _ -> return Results

let verbatim =
  lines_starts_with (char ':') <?> "verbatim"

let md_blockquote =
  lines_starts_with (char '>') <?> "markdown blockquote"

(* ``` json
 * {
 *   "firstName": "John",
 *   "lastName": "Smith",
 *   "age": 25
 * }
 * ``` *)
let fenced_language =
  (string "```" <|> string "~~~") *> spaces *> optional line

let fenced_code_block =
  fenced_language
  >>= fun language ->
  between_lines ~trim:false (fun line ->
      (starts_with (String.trim line) "```") || (starts_with (String.trim line) "~~~")
    ) "fenced_code_block"
  >>| fun lines ->
  (* clear indents *)
  let lines = if lines = [] then [] else
      let indent = get_indent (List.hd lines) in
      if indent = 0 then lines else
        List.map (fun line ->
            Prelude.safe_sub line indent (String.length line - indent)
          ) lines in
  Src {language; options=None; lines}

let block_name_options_parser =
  lift2 (fun name options ->
      match options with
      | None | Some "" -> (name, None)
      | _ -> (name, options))
    (string_ci "#+begin_" *> non_spaces)
    (spaces *> optional line)
  <* (optional eol)

let list_content_parsers config block_parse =
  many1 (choice [
      Table.parse config
    ; block_parse
    ; Directive.parse
    ; Drawer.parse
    ; Latex_env.parse config
    ; Hr.parse config
    ; results
    ; Comment.parse config
    ; Paragraph.parse config [ Table.parse config
                             ; Drawer.parse
                             ; block_parse
                             ; Directive.parse
                             ; Latex_env.parse config
                             ; Hr.parse config
                             ; results
                             ; Comment.parse config]
    ])

let block_content_parsers config block_parse =
  let list_content_parser = list_content_parsers config block_parse in
  many1 (choice [
      Directive.parse
    ; Table.parse config
    ; Lists.parse config list_content_parser
    ; Drawer.parse
    ; block_parse
    ; Latex_env.parse config
    ; Hr.parse config
    ; results
    ; Comment.parse config
    ; Paragraph.parse config [ Table.parse config
                             ; Lists.parse config list_content_parser
                             ; Drawer.parse
                             ; block_parse
                             ; Directive.parse
                             ; Latex_env.parse config
                             ; Hr.parse config
                             ; results
                             ; Comment.parse config]
    ])

let separate_name_options = function
  | None -> (None, None)
  | Some s ->
    match String.split_on_char ' ' s with
    | [] -> (None, None)
    | name :: [] -> (Some name, None)
    | name :: options -> (Some name, Some options)

let block_parse config = fix (fun parse ->
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
        let lines = if lines = [] then [] else
            let indent = get_indent (List.hd lines) in
            if indent = 0 then lines else
              List.map (fun line ->
                  Prelude.safe_sub line indent (String.length line - indent)
                ) lines in
        let name = String.lowercase_ascii name in
        (match name with
         | "src" ->
           let (language, options) = separate_name_options options in
           Src {language; options; lines}
         | "example" -> Example lines
         | "quote" ->
           let content = String.concat "\n" lines in
           let result = match parse_string (block_content_parsers config parse) content with
             | Ok result -> result
             | Error _e -> [] in
           Quote (List.concat result)
         | "export" ->          (* export html, etc *)
           let (name, options) = separate_name_options options in
           let name = match name with None -> "" | Some s -> s in
           let content = String.concat "\n" lines in
           Export (name, options, content)
         | "comment" ->
           CommentBlock lines
         | _ ->
           let content = String.concat "\n" lines in
           let result = match parse_string (block_content_parsers config parse) content with
             | Ok result -> List.concat result
             | Error _e -> [] in
           Custom (name, options, result, content)
        )
      | ':' ->                      (* verbatim block *)
        begin match config.format with
          | "Org" ->
            verbatim >>|
            fun lines -> Example lines
          | "Markdown" ->
            fail "block"
        end
      | '>' ->
        md_blockquote >>|
        fun lines ->
        let content = String.concat "\n" lines in
        let result = match parse_string (block_content_parsers config parse) content with
          | Ok result -> result
          | Error _e -> [] in
        Quote (List.concat result)
      | '`' | '~' ->
        fenced_code_block
      | '<' ->
        Raw_html.parse >>| fun s -> Raw_Html s
      | '[' ->
        Hiccup.parse >>| fun s -> Hiccup s
      | _ -> fail "block" in
    between_eols p)

let parse config =
  match config.format with
  | "Org" ->
    block_parse config
  | "Markdown" ->
    block_parse config <|> Markdown_code_block.parse

open! Prelude
open Angstrom
open Parsers
open Type
open Conf
open Helper
open Pos

module MakeBlock (Lists : sig
  val parse :
    Conf.t -> (Type.t * Pos.pos_meta) list Angstrom.t -> Type.t Angstrom.t
end) =
struct
  (* There are 2 kinds of blocks.
     1. `begin ... end`
     #+BEGIN_X
     line1
     line 2
     #+END_x

     2. Verbatim, each line starts with `:`.
  *)

  let results = spaces *> string "#+RESULTS:" >>= fun _ -> return Results

  let verbatim = lines_starts_with (char ':') <?> "verbatim"

  let md_blockquote = lines_starts_with (char '>') <?> "markdown blockquote"

  let displayed_math =
    string "$$" *> end_string "$$" (fun s -> Displayed_Math s)

  (* ``` json
   * {
   *   "firstName": "John",
   *   "lastName": "Smith",
   *   "age": 25
   * }
   * ``` *)
  let fenced_language =
    (string "```" <|> string "~~~") *> spaces *> optional line <* optional eol

  let fenced_code_block =
    fenced_language >>= fun language ->
    let p =
      between_lines ~trim:false
        (fun line ->
          starts_with (String.trim line) "```"
          || starts_with (String.trim line) "~~~")
        "fenced_code_block"
    in
    let p' = with_pos_meta p in
    p' >>| fun (lines, { start_pos; end_pos }) ->
    (* clear indents *)
    let lines =
      if lines = [] then
        []
      else
        let indent = get_indent (List.hd lines) in
        if indent = 0 then
          lines
        else
          List.map
            (fun line -> safe_sub line indent (String.length line - indent))
            lines
    in
    let pos_meta = { start_pos; end_pos = end_pos - 3 } in
    Src { language; options = None; lines; pos_meta }

  let block_name_options_parser =
    lift2
      (fun name options ->
        match options with
        | None
        | Some "" ->
          (name, None)
        | _ -> (name, options))
      (string_ci "#+begin_" *> non_spaces)
      (spaces *> optional line)
    <* optional eol

  let list_content_parsers config block_parse =
    let p =
      choice
        [ Table.parse config
        ; block_parse
        ; Directive.parse
        ; Latex_env.parse config
        ; Hr.parse config
        ; results
        ; Comment.parse config
        ; Paragraph.parse
        ; Paragraph.sep
        ]
    in
    let p = Helper.with_pos_meta p in
    many1 p

  let block_content_parsers config block_parse =
    let list_content_parser = list_content_parsers config block_parse in
    let p =
      choice
        [ Directive.parse
        ; Table.parse config
        ; Lists.parse config list_content_parser
        ; block_parse
        ; Latex_env.parse config
        ; Hr.parse config
        ; results
        ; Comment.parse config
        ; Paragraph.parse
        ; Paragraph.sep
        ]
    in
    let p = Helper.with_pos_meta p in
    many1 p

  let separate_name_options = function
    | None -> (None, None)
    | Some s -> (
      match String.split_on_char ' ' s with
      | [] -> (None, None)
      | [ name ] -> (Some name, None)
      | name :: options -> (Some name, Some options))

  let block_parse config =
    fix (fun parse ->
        let p =
          peek_char_fail >>= function
          | '#' -> (
            block_name_options_parser >>= fun (name, options) ->
            let p =
              between_lines ~trim:false
                (fun line ->
                  let prefix = "#+end_" ^ name in
                  starts_with (String.trim line) prefix)
                "block"
            in
            let p' = with_pos_meta p in
            p' >>| fun (lines, { start_pos; end_pos }) ->
            (* clear indents *)
            let lines =
              if lines = [] then
                []
              else
                let indent = get_indent (List.hd lines) in
                if indent = 0 then
                  lines
                else
                  List.map
                    (fun line ->
                      safe_sub line indent (String.length line - indent))
                    lines
            in
            let name = String.lowercase_ascii name in
            match name with
            | "src" ->
              let language, options = separate_name_options options in
              let pos_meta = { start_pos; end_pos = end_pos - 9 } in
              Src { language; options; lines; pos_meta }
            | "example" -> Example lines
            | "quote" ->
              let content = String.concat "" lines in
              let result =
                match
                  parse_string ~consume:All
                    (block_content_parsers config parse)
                    content
                with
                | Ok result ->
                  let result = Paragraph.concat_paragraph_lines config result in
                  List.map fst result
                | Error _e -> []
              in
              Quote result
            | "export" ->
              (* export html, etc *)
              let name, options = separate_name_options options in
              let name =
                match name with
                | None -> ""
                | Some s -> s
              in
              let content = String.concat "" lines in
              Export (name, options, content)
            | "comment" -> CommentBlock lines
            | _ ->
              let content = String.concat "" lines in
              let result =
                match
                  parse_string ~consume:All
                    (block_content_parsers config parse)
                    content
                with
                | Ok result ->
                  let result = Paragraph.concat_paragraph_lines config result in
                  List.map fst result
                | Error _e -> []
              in
              Custom (name, options, result, content))
          | ':' -> (
            (* verbatim block *)
            match config.format with
            | Org -> verbatim >>| fun lines -> Example lines
            | Markdown -> fail "block")
          | '>' ->
            md_blockquote >>| fun lines ->
            let content = String.concat "" lines in
            let result =
              match
                parse_string ~consume:All
                  (block_content_parsers config parse)
                  content
              with
              | Ok result ->
                let result = Paragraph.concat_paragraph_lines config result in
                List.map fst result
              | Error _e -> []
            in
            Quote result
          | '`'
          | '~' ->
            fenced_code_block
          | '$' -> displayed_math
          | '<' -> Raw_html.parse >>| fun s -> Raw_Html s
          | '[' ->
            if config.hiccup_in_block then
              Hiccup.parse >>| fun s -> Hiccup s
            else
              fail "block"
          | _ -> fail "block"
        in
        between_eols p)

  let parse config =
    match config.format with
    | Org -> block_parse config
    | Markdown -> block_parse config
end

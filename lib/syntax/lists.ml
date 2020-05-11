open Angstrom
open Parsers
open Prelude
open Type
open Conf

let indent_parser = (peek_spaces >>| (function s -> String.length s)) <|> return 0

let check_listitem line =
  let indent = get_indent line in
  let number =
    try
      Scanf.sscanf (String.trim line) "%d" (fun x -> Some x)
    with _ -> None in
  match number with
  | Some number ->
    (indent, true, Some number)
  | None ->
    if (String.length line) - indent >= 2 then
      let prefix = String.sub line indent 2 in
      (indent, prefix = "- " || prefix = "+ " || (indent <> 0 && prefix = "* "), None)
    else
      (indent, false, None)

let terminator items =
  if !items = [] then
    fail "list"
  else
    let result = ! items in
    return @@ List.rev result

let content_parser list_parser content_parsers indent lines =
  fix (fun content_parser ->
      take_till1 is_eol
      >>= fun content ->
      lines := content :: !lines;
      two_eols (List.rev !lines, []) (* two newlines end this list *)
      <|>
      optional eol *>
      (peek_char >>= function
        | None ->
          return (List.rev !lines, [])
        | Some c ->
          if is_eol c then (
            lines := "\n" :: !lines;
            eol *> content_parser
          ) else if is_space c then (
            peek_line >>= fun content ->
            let (indent', is_item, _number) = check_listitem content in
            if is_item then (
              if indent' <= indent then (* breakout, another item or a new list. *)
                return (List.rev !lines, [])
              else                      (* list item child *)
                list_parser content_parsers (ref []) indent' >>= fun items ->
                return (List.rev !lines, items)
            ) else (                    (* content of current item *)
              optional eols *> content_parser))
          else (
            return (List.rev !lines, []))))

let format_parser _config indent =
  let choices = if indent = 0 then
      char '+' <|> char '-' else
      char '+' <|> char '-' <|> char '*'
  in
  let unordered_format = (choices *> ws *> return None) in
  let ordered_format = (digits <* char '.' <* ws >>=
                        fun number -> return (Some number)) in
  unordered_format <|> ordered_format

let checkbox_parser =
  (string "[ ]" *> return (Some false))
  <|>
  (string_ci "[X]" *> return (Some true))
  <|>
  return None

let format_checkbox_parser config indent =
  lift2 (fun format checkbox ->
      (format, checkbox))
    (format_parser config indent)
    (checkbox_parser <* spaces)

(* name :: definition *)
let definition s =
  let name_parser = (end_string " ::" (fun s -> s)) in
  match parse_string name_parser s with
  | Ok name ->
    let l = (String.length name + 3) in
    let nc = String.get s l in
    if is_space nc || is_eol nc then
      (Some name, String.sub s l (String.length s - l))
    else
      (None, s)
  | Error _e ->
    (None, s)

let rec list_parser config content_parsers items last_indent =
  fix (fun list ->
      (indent_parser >>= fun indent ->
       if last_indent > indent then
         terminator items       (* breakout *)
       else
         let content_parser number checkbox =
           content_parser (list_parser config) content_parsers indent (ref []) >>= fun (content, children) ->
           let ordered =
             match number with Some _ -> true | None -> false
           in
           let content = List.map String.trim content in
           let content = String.concat "\n" content in
           let (name, content) = if ordered then (None, content) else (definition content) in
           let content = match parse_string content_parsers content with
             | Ok result -> List.concat result
             | Error _e -> [Paragraph [Inline.Plain content]]
           in
           let item = {content; name; items=children; number; checkbox; indent; ordered} in
           items := item :: !items;
           list in
         Angstrom.take indent *> (* skip indent *)
         (format_checkbox_parser config indent >>= fun (number, checkbox) ->
          match number with
          | None -> content_parser None checkbox
          | Some number ->
            content_parser (Some (int_of_string number)) checkbox)
         <|>
         terminator items       (* breakout *)
      ))

let org_parse config content_parsers =
  let r = ref [] in
  let p = list_parser config content_parsers r 0 in
  optional eols *> p >>= fun result ->
  r := [];
  return [List result]
  <|>
  let _ = r := [] in
  fail "list"

let parse config content_parsers =
  match config.format with
  | "Org" -> org_parse config content_parsers
  | "Markdown" ->
    (org_parse config content_parsers)
    <|>
    fail "markdown"

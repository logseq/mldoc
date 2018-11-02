open Angstrom
open Parsers
open Prelude
open Org

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
    if String.length line > 2 then
      let prefix = String.sub line indent 2 in
      (indent, prefix = "- " || prefix = "+ ", None)
    else
      (indent, false, None)

let content_parser list_parser indent lines =
  fix (fun content_parser ->
      line >>= fun content ->
      lines := content :: !lines;
      two_eols (List.rev !lines, [])
      <|>
      (optional eol >>= function
        | None ->
          return (List.rev !lines, [])
        | Some eol -> peek_char >>= function
          | None -> return ((List.rev !lines), [])
          | Some c ->
            if is_space c then (
              peek_line >>= fun content ->
              let (indent', is_item, number) = check_listitem content in
              if is_item then (
                if indent' < indent then
                  return @@ (List.rev !lines, [])
                else if indent' = indent then
                  return @@ (List.rev !lines, [])
                else            (* list item child *)
                  list_parser (ref []) indent' >>= fun items ->
                  return @@ (List.rev !lines, items)
              ) else (
                line >>= fun content ->
                lines := content :: !lines;
                content_parser))
            else (
              return ((List.rev !lines), []))))

let terminator items =
  if !items = [] then
    fail "list"
  else
    let result = ! items in
    return @@ List.rev result

(*
- item 1
- item 2


- item 1
 hello world
 - item 1.1
 - item 1.2
 + item 1.3
  1. item 1.3.1
  2. item 1.3.2
   1.3.2 content
   This is beautiful
- item 2
 item 2 content
*)
let checkbox_parser =
  (string "[ ]" *> return (Some false))
  <|>
  (string_ci "[X]" *> return (Some true))
  <|>
  return None

let rec list_parser items last_indent =
  fix (fun list ->
      (indent_parser >>= fun indent ->
       if last_indent > indent then
         terminator items
       else
         let format_checkbox_parser = lift2 (fun format checkbox ->
             (format, checkbox))
             (non_spaces <* spaces)
             (checkbox_parser <* spaces) in
         let content_parser number checkbox =
           content_parser list_parser indent (ref []) >>= fun (content, children) ->
           let ordered =
             match number with Some _ -> true | None -> false
           in
           let item = {content; items=children; number; checkbox; indent; ordered} in
           items := item :: !items;
           list in
         Angstrom.take indent *> (* skip indent *)
         (format_checkbox_parser >>= fun (format, checkbox) ->
          match format with
          | "-" | "+" ->
            content_parser None checkbox
          | ordered when is_ordered ordered ->
            (match Scanf.sscanf ordered "%d" (fun x -> Some x) with
             | Some number ->
               content_parser (Some number) checkbox
             | None ->
               content_parser (Some 0) checkbox)
          | _ ->
            terminator items)
         <|>
         terminator items))

let parse =
  let r = ref [] in
  optional eols *> list_parser r 0 >>= fun result ->
  r := [];
  return [List result]
  <|>
  let _ = r := [] in
  fail "list"

(*
Lists can take a lot of different forms in =org-mode=
*** Unnumbered lists
They are the simplest kind of lists. They consist in list of indented
items starting by =-= or =+=:
#+BEGIN_EXAMPLE
- This is one item,
  still the continuation of the first item (note the indentation)

- Another item.
#+END_EXAMPLE
Items are may or may not be separated by newlines. Items can
themselves contain block content, for example lists:

#+BEGIN_EXAMPLE
- This is a list item.
  - A list in a list
  - Second item.
    - Note the indentation: twice for each level.
- Second item of the outermost list.
#+END_EXAMPLE
yields
#+BEGIN_QUOTE
- This is a list item.
  - A list in a list
  - Second item.
    - Note the indentation: twice for each level.
- Second item of the outermost list.
#+END_QUOTE

List items do not stop after one empty line but two, allowing to
typeset things like that:

#+BEGIN_EXAMPLE
- This is a first paragraph of a list item.

  This is a second one.
#+END_EXAMPLE
to get
#+BEGIN_QUOTE
+ This is a first paragraph of a list item.

  This is a second one.
#+END_QUOTE

To be clear: normally it would have been necessary to indent the empty
line (ie. make a line containing only two spaces). When dealing with
complicated nested lists, it is /advised/ to indent empty line to be
sure of the result.

On top of that, list items may start with a checkbox of the form =[ ]=
or =[X]=.
#+BEGIN_EXAMPLE
- [ ] Not done item
- [X] Done item
#+END_EXAMPLE
gives
#+BEGIN_QUOTE
- [ ] Not done item
- [X] Done item
#+END_QUOTE
These checkboxes are often ignored by exporters.

*** Numbered lists
Numbered lists are lists whose items start with a number and a dot, for instance:
#+BEGIN_EXAMPLE
1. First item
1. Second item (note that the number before the dot is useless)
#+END_EXAMPLE
gives
#+BEGIN_QUOTE
1. First item
1. Second item (note that the number before the dot is useless)
#+END_QUOTE

*)

(* not works *)
(* parse_string parse "1. [ ] Not done item
 * 2. [X] Done item
 *   - great"
 * ;; *)

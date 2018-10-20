open Angstrom
open Parsers
open Prelude

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

type t =
  {
    title: string option              (* item title *)
  ; content: string list option  (** The contents of the current item *)
  ; items: t list option
  ; number: int option  (** Its number *)
  ; checkbox: bool option  (** Was it checked *)
  ; ordered: bool  (** Is the list ordered *)
  ; indent: int option (** Indentation of the current item. *)
  }

type 'a orglist = None | Some of (int * string list * 'a orglist) list

let indent_parser = (peek_spaces >>| (function s -> String.length s)) <|> return 0

let two_newlines result = (count 2 eol >>= fun _ -> return result )

let check_listitem line =
  let indent = get_indent line in
  if Str.string_match (Str.regexp "^[0-9]+\\. ") (String.trim line) 0 then
    (indent, true, true)
  else
    if String.length line > 2 then
      let prefix = String.sub line indent 2 in
      (indent, prefix = "- " || prefix = "+ ", false)
    else
      (indent, false, false)


let content_parser list_parser indent lines =
  fix (fun content_parser ->
      line >>= fun content ->
      lines := content :: !lines;
      two_newlines ((List.rev !lines), None)
      <|>
      (optional eol >>= function
        | None ->
          return (List.rev !lines, None)
        | Some eol -> peek_char >>= function
          | None -> return ((List.rev !lines), None)
          | Some c ->
            if is_space c then (
              peek_line >>= fun content ->
              let (indent', is_item, ordered) = check_listitem content in
              if is_item then (
                if indent' < indent then
                  return @@ (List.rev !lines, None)
                else if indent' = indent then
                  return @@ (List.rev !lines, None)
                else            (* list item child *)
                  list_parser (ref []) indent' >>= fun items ->
                  return @@ (List.rev !lines, Some items)
              ) else (
                line >>= fun content ->
                lines := content :: !lines;
                content_parser))
            else (
              return ((List.rev !lines), None))))

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
let rec list_parser items last_indent =
  fix (fun list ->
      (indent_parser >>= fun indent ->
       if last_indent > indent then
         terminator items
       else
         Angstrom.take indent *> (* skip indent *)
         (non_spaces <* spaces >>=
          let content_parser = content_parser list_parser indent (ref []) >>= fun (content, children) ->
            items := (indent, content, children) :: !items;
            list in
          function
          | "-" | "+" ->
            content_parser
          | ordered when is_ordered ordered ->
            content_parser
          | _ ->
            terminator items)
         <|>
         terminator items))

let list =
  let r = ref [] in
  list_parser r 0 >>= fun result ->
  r := [];
  return result
  <|>
  let _ = r := [] in
  fail "list"

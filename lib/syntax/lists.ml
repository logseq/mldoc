open Angstrom
open Parsers

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
=mlorg= allow to customize the format of the number of the list by a
directive of the form =[@format]=. =format= can be any string, in which:
- =i= is replaced by the item number spelled in lowercase latin number
- =I= is replaced by the item number spelled in uppercase latin number
- =α= is replaced by the item number spelled in uppercase greek number
- =Α= is replaced by the item number spelled in uppercase greek number (Note: this is a capital alpha!)
- =1= is replaced by the item number spelled in arabic digit

Of course, these strings represent all the same number (namely 1) in
their respective representation, but you can choose any number you
like. If so it will set up the number item to this number. (If they
are several number, the first one is picked)
#+BEGIN_EXAMPLE
1. [@(i)] Lower case latin
2. Two
1. [@III.] Upper case latin
1. Four
1. [@{δ}] What's after δ ?
1. ε !
1. [@42.] Numbers
1. [@1 ii III δ] Who will win ?
1. Did you guess right ?
#+END_EXAMPLE
#+BEGIN_QUOTE
1. [@(i)] Lower case latin
2. Two
1. [@III.] Upper case latin
1. Four
1. [@{δ}] What's after δ ?
1. ε !
1. [@42.] Numbers
1. [@1 ii III δ] Who will win ?
1. Did you guess right ?
#+END_QUOTE

See the module {{{doc(Numbering)}}} for more information.
*)

type t =
  {
    children: t list option
  ; content: string  (** The contents of the current item *)
  ; number: int option  (** Its number *)
  ; format: string option (** Its format *)
  ; checkbox: bool option  (** Was it checked *)
  ; ordered: bool  (** Is the list ordered *)
  ; indent: int (** Indentation of the current item. *)
  }

let indent_parser = optional ws >>| (function
    | None -> 0
    | Some s -> String.length s) <?> "indent_parser"

let two_newlines result = (count 2 eol >>= fun _ -> return result )

let terminator items =
  if !items = [] then
    fail "list"
  else
    return @@ List.rev !items

let check_listitem line =
  let prefix = String.sub (String.trim line) 0 2 in
  prefix = "- " || prefix = "+ "

let content_parser list_parser items indent lines =
  fix (fun content_parser ->
      line >>= fun content ->
      lines := content :: !lines;
      two_newlines (List.rev !lines)
      <|>
      (optional eol >>= function
        | None -> return (List.rev !lines)
        | Some eol -> peek_char >>= function
          | None -> return (List.rev !lines)
          | Some c ->
            if is_space c then
              line >>= fun content ->
              (* check for nested list item *)
              if check_listitem content then (
                terminator lines
              )
              else (
                lines := content :: !lines;
                content_parser)
            else
              return (List.rev !lines)))
(*
- item 1
 hello world
 - item 1.1
 - item 1.2
- item 2
*)
let list items =
  fix (fun list ->
      (indent_parser >>= fun indent ->
       (non_spaces <* spaces >>= function
           (* TODO: ordered list, numbers, or other formats *)
         | "-" | "+" ->
           content_parser list items indent (ref []) >>= fun lines ->
           items := (indent, lines) :: !items;
           list
         | _ ->
           terminator items)
       <|>
       terminator items))

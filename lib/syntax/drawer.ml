(*
Drawers a way to hide information in =org-mode=. The syntax is:
:DRAWERNAME:
Contents of the drawer (socks for instance)
:END:

There is a special kind of drawer that =mlorg= recognizes, it is the
/PROPERTY/ drawer, which look like:
: :PROPERTIES:
: :KEY: Value
: :KEY: Value
: :KEY: Value
: :END:
They are used to store information about a heading and can be used to
filter on them. (Exporters don't use them as of today)
*)

open Angstrom
open Parsers
open Types

let end_mark = ":END:"


let parse =
  let drawer_name =
    optional ws *>
    between_char ':' ':' (take_while1 (fun c -> c <> ':')) <* eol in
  let drawer_body =
    between_lines (fun line -> line = end_mark) "drawer body" in
  (* anything but a headline and another drawer *)
  optional eols *>
  lift2 (fun name body ->
      [Drawer (name, body)]) drawer_name drawer_body

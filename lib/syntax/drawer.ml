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
open Org

let end_mark = ":END:"

let drawer_body =
  let p lines =
    fix (fun body_parser ->
        optional eols *> take_till1 is_eol <* optional eols >>= fun line ->
        let line = String.trim line in
        if line = end_mark then
          return (List.rev !lines)
        else
          let _ = lines := line :: !lines in
          body_parser) in
  clear_parser_resource p (ref []) "drawer body"

let parse =
  let drawer_name = optional ws *> between_char ':' ':' (take_while1 (fun c -> c <> ':')) <* eol in
  (* anything but a headline and another drawer *)
  optional eols *>
  lift2 (fun name body ->
      [Drawer (name, body)]) drawer_name drawer_body

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
open Type

let end_mark = ":END:"

let property =
  let property_key = spaces *> between_char ':' ':' (take_while1 (fun c -> c <> ':' && c <> ' ' && c <> '\n')) in
  let property_value = spaces *> line <* eol in
  lift2 (fun key value ->
      (key, value)
    )
    property_key property_value

let drawer_properties = many1 property

(* TODO: support other drawers than properties *)
let parse =
  let drawer_name =
    spaces *>
    between_char ':' ':' (take_while1 (fun c -> c <> ':' && c <> ' ' && c <> '\n')) <* eol in
  (* anything but a headline and another drawer *)
  let p =
    lift2 (fun name properties ->
        match String.lowercase_ascii(name) with
          "properties" ->
          Property_Drawer properties
        | _ -> Drawer (name, properties))
      drawer_name drawer_properties in
  p <* string_ci end_mark

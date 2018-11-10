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

let parse_properties =
  List.fold_left
    (fun acc line ->
       try
         Scanf.sscanf (String.trim line) ":%[^:]: %[^\n]" (fun key value ->
             (key, value) :: acc )
       with _ -> (
           match acc with
           | [] ->
             acc
           | (key, v) :: acc' ->
             (* Because line might be indented *)
             let line = " " ^ String.trim line in
             (key, v ^ line) :: acc' ) )
    []

let parse =
  let drawer_name =
    spaces *>
    between_char ':' ':' (take_while1 (fun c -> c <> ':')) <* eol in
  let drawer_body =
    between_lines (fun line -> line = end_mark) "drawer body" in
  (* anything but a headline and another drawer *)
  let p =
    lift2 (fun name body ->
        let drawer = match name with
            "PROPERTIES" ->
            let properties = parse_properties body in
            Property_Drawer (List.rev properties)
          | _ -> Drawer (name, body) in
        [drawer]) drawer_name drawer_body in
  between_eols_or_spaces p

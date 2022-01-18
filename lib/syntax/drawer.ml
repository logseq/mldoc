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
open! Prelude
open Angstrom
open Parsers
open Type

let end_mark = ":END:"

let property =
  let property_key =
    optional spaces
    *> between_char ':' ':'
         (take_while1 (fun c -> c <> ':' && c <> ' ' && non_eol c))
    >>= fun s ->
    if String.lowercase_ascii s = "end" then
      fail "property key"
    else
      return s
  in
  let property_value =
    optional spaces *> optional_line <* (eol *> return () <|> end_of_input)
  in
  lift2 (fun key value -> (key, value)) property_key property_value

let drawer_properties = many property

let drawer_name =
  spaces
  *> between_char ':' ':'
       (take_while1 (fun c -> c <> ':' && c <> ' ' && non_eol c))
  <* eol

let parse1 config =
  let is_markdown = Conf.is_markdown config in
  (* anything but a headline and another drawer *)
  let p =
    let name = spaces *> string_ci ":PROPERTIES:" <* eol in
    lift2
      (fun _name properties -> Property_Drawer properties)
      name drawer_properties
  in
  let p' = p <* spaces <* string_ci end_mark <* optional eol in
  if is_markdown then
    Markdown_property.parse <|> p'
  else
    p'

(* #+NAME: VALUE like orgmode property *)
let name =
  between_string "#+" ":" (take_while1 (fun c -> c <> ':' && non_space_eol c))

let parse2 =
  let p =
    lift2
      (fun name value -> Property_Drawer [ (name, value) ])
      name (spaces *> optional_line)
  in
  between_eols p

let drawer_content =
  between_lines ~trim:false
    (fun line ->
      String.equal (String.lowercase_ascii (String.trim line)) ":end:")
    "drawer_content"

let drawer_parse =
  let p =
    lift2
      (fun name content ->
        let name = String.lowercase_ascii name in
        let content = remove_last_newlines content in
        Drawer (name, content))
      drawer_name drawer_content
  in
  p <* optional eol

let parse_to_kvs config : (string * string) list Angstrom.t =
  many1 (parse1 config <|> parse2) >>| fun properties ->
  List.fold_left
    (fun r e ->
      match e with
      | Property_Drawer kvs -> List.append r kvs
      | _ -> failwith "unreachable")
    [] properties

(* combine
   :PROPERTIES: :END: properties and #+NAME: VALUE properties *)
let parse config =
  many1 (parse1 config <|> parse2)
  >>= (fun properties ->
        return
        @@ Property_Drawer
             (List.fold_left
                (fun r e ->
                  match e with
                  | Property_Drawer kvs -> List.append r kvs
                  | _ -> failwith "unreachable")
                [] properties))
  <|> drawer_parse

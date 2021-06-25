type format =
  | Org
  | Markdown

let format_to_yojson format =
  match format with
  | Org -> `String "Org"
  | Markdown -> `String "Markdown"

let format_of_yojson json =
  match json with
  | `String "Org" -> Ok Org
  | `String "Markdown" -> Ok Markdown
  | _ -> Error "invalid format"

type t =
  { (* html: bool; *)
    (* hiccup: bool; *)
    toc : bool
  ; heading_number : bool
  ; keep_line_break : bool (* FIXME: is this option deprecated? *)
  ; format : format
  ; heading_to_list : bool (* export heading as list *)
  ; exporting_keep_properties : bool
        [@default false] (* keep properties when exporting *)
  ; ignore_heading_list_marker : bool [@default false]
  ; inline_type_with_pos : bool [@default false]
  }
[@@deriving yojson]

let is_markdown t = t.format = Markdown

let is_org t = t.format = Org

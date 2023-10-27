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

type indent_style =
  | Dashes
  | Spaces
  | NoIndent

let indent_style_to_yojson = function
  | Dashes -> `String "dashes"
  | Spaces -> `String "spaces"
  | NoIndent -> `String "no-indent"

let indent_style_of_yojson = function
  | `String s -> (
    match String.lowercase_ascii s with
    | "dashes" -> Ok Dashes
    | "spaces" -> Ok Spaces
    | "no-indent" -> Ok NoIndent
    | _ -> Ok Dashes)
  | _ -> Ok Dashes

type meta_chars =
  | Page_ref (* [[text]] *)
  | Emphasis (* **text**, __text__, ... *)
[@@deriving yojson]

type t =
  { (* html: bool; *)
    (* hiccup: bool; *)
    toc : bool [@default false]
  ; parse_outline_only : bool [@default false]
  ; heading_number : bool [@default false]
  ; keep_line_break : bool (* FIXME: is this option deprecated? *)
  ; format : format
  ; heading_to_list : bool [@default false] (* export heading as list *)
  ; exporting_keep_properties : bool
        [@default false] (* keep properties when exporting *)
  ; inline_type_with_pos : bool [@default false]
  ; inline_skip_macro: bool [@default false]
  ; export_md_indent_style : indent_style [@default Dashes]
  ; export_md_remove_options : meta_chars list [@default []]
  ; hiccup_in_block : bool [@default true]
  ; enable_drawers : bool [@default true]
  }
[@@deriving yojson]

let is_markdown t = t.format = Markdown

let is_org t = t.format = Org

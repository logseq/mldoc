type format = Org | Markdown [@@deriving yojson]
type t = {
    toc: bool;
    heading_number: bool;
    keep_line_break: bool;
    format: format;
  } [@@deriving yojson]

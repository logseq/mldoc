type format = Org | Markdown [@@deriving yojson]
type t = {
    (* html: bool; *)
    (* hiccup: bool; *)
    toc: bool;
    heading_number: bool;
    keep_line_break: bool;
    format: string;
  } [@@deriving yojson]

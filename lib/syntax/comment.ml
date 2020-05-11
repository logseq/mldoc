open Angstrom
open Parsers
open Type
open Conf

let org_parse =
  let p =
    (char '#' <* optional spaces) *>
    line >>= function s ->
      return [Comment s]
  in between_eols p

let parse config =
  match config.format with
  | "Org" -> org_parse
  | "Markdown" -> Markdown_comment.parse

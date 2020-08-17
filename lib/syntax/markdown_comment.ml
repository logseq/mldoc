open Angstrom
open Parsers
open Type

(* <!---
 * All this should be
 * commented out
 * -->
 *
 * <!--
 * This too
 * --> *)
(* We don't support inline comment like <!-- comment --> for now. *)
let html_comment =
  choice [string "<!---"; string "<!--"]
  <* end_of_line
  >>= function _ ->
    between_lines ~trim:false (fun line ->
        let prefix = "-->" in
        String.equal (String.trim line) prefix) "markdown_comment"
    >>= fun lines ->
    let content = String.concat "\n" lines in
    return @@ Comment content

(* [//]: #  *)
let comment =
  let prefix = optional spaces *> string "[//]: #" <* optional spaces in
  lift2 (fun _ s -> Comment s) prefix line

let parse = choice [html_comment; comment]

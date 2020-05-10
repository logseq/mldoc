open Angstrom
open Parsers
open Type

let org_parse =
  let p =
    char '#' *>
    peek_char >>= function
    | None ->
      return [Comment ""]
    | Some c ->
      if is_space c || is_eol c then
        optional line >>= fun line ->
        let line = match line with None -> "" | Some s -> s in
        return [Comment line]
      else
        fail "comment"
  in between_eols p

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
        print_endline line;
        let prefix = "-->" in
        String.equal (String.trim line) prefix) "markdown_comment"
    >>= fun lines ->
    let content = String.concat "\n" lines in
    return [Comment content]

(* [//]: #  *)
let markdown_comment =
  let prefix = optional spaces *> string "[//]: #" <* optional spaces in
  lift2 (fun _ s -> [Comment s] ) prefix line

let markdown_parse = choice [html_comment; markdown_comment]
let parse = choice [org_parse; markdown_parse]

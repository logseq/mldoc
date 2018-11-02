open Angstrom
open Parsers
open Prelude
open Org

(* There are 2 kinds of blocks.
   1. `begin ... end`
   #+BEGIN_X
   line1
   line 2
   #+END_x

   2. Verbatim, each line starts with `:`.
*)

let verbatim lines =
  fix (fun verbatim ->
      spaces *> char ':' *> ws *> take_till is_eol <* optional eol
      >>= fun line ->
      lines := line :: !lines;
      verbatim
      <|>
      (if !lines = [] then
         fail "verbatim"
       else
         return !lines))

let parse =
  spaces *> peek_char_fail
  >>= function
  | '#' ->
    string_ci "#+begin_" *>
    non_spaces <* eol     (* name *)
    >>= fun name ->
    (* TODO: example, src, custom *)
    between_lines (fun line ->
        let prefix = "#+end_" ^ name in
        starts_with line prefix) "block"
    >>= fun lines ->
    return (String.lowercase_ascii name, lines)
  | ':' ->                      (* verbatim block *)
    verbatim (ref []) >>|
    fun lines -> ("", List.rev lines)
  | _ -> fail "block"

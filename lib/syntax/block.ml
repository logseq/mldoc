open Angstrom
open Parsers
open Prelude

(* There are 2 kinds of blocks.
   1. `begin ... end`
   #+BEGIN_X
   line1
   line 2
   #+END_x

   2. Verbatim, each line starts with `:`.
*)

let clear_indents s =
  let lines = String.split_on_char '\n' s in
  let lines = List.map String.trim lines in
  take (List.length lines - 1) lines

let verbatim lines =
  fix (fun verbatim ->
      spaces *> char ':' *> ws *> take_till is_eol
      >>= fun line ->
      lines := line :: !lines;
      verbatim
      <|>
      (if !lines = [] then
         fail "verbatim"
       else
         return !lines
      ))

let parse =
  spaces *> peek_char_fail
  >>= function
  | '#' ->
    string_ci "#+begin_" *>
    non_spaces <* eol     (* name *)
    >>= fun name ->
    end_string ("#+end_" ^ name) ~ci:true (fun s -> (String.lowercase_ascii name, clear_indents s))
  | ':' ->                      (* verbatim block *)
    verbatim (ref []) >>|
    fun lines -> ("", List.rev lines)
  | _ -> fail "block"

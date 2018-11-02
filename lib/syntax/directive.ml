(*
#+TITLE: Test
#+AUTHOR: Testman
#+MACRO: macro-name Contents which can refer to argument using $1, ..., $k

Example:
: #+MACRO: test Some /$1/ *$2*
: {{{test(Macro, invocation)}}}
gives
#+MACRO: test Some /$1/ *$2*
*)

open Angstrom
open Parsers
open Org

let parse =
  optional eols *>
  lift2 (fun name value -> [Directive (name, value)])
    (between_string "#+" ":" (take_while1 (fun c -> c <> ':')))
    (optional ws *> line)

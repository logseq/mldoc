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
open Type
open Prelude

let name =
  (between_string "#+" ":" (take_while1 (fun c -> c <> ':' && non_eol c)))
  >>= fun s ->
  if starts_with s "BEGIN_" || starts_with s "begin_" then
    fail "Directive might be a block"
  else
    return s

let parse =
  let p =
    lift2 (fun name value -> [Directive (name, value)])
      name
      (spaces *> optional_line) in
  between_eols p

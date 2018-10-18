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

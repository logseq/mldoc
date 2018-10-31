open Angstrom
open Org

(* inline and footnotes *)

let parse = Inline.parse >>=
  fun inlines -> return (Paragraph inlines)

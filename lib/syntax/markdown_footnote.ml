open Angstrom
open Parsers

(*
Here's a simple footnote,[^1] and here's a longer one.[^bignote]

[^1]: This is the first footnote.

[^bignote]: Here's one with multiple paragraphs and code.

    Indent paragraphs to include them in the footnote.

    `{ my code }`

    Add as many paragraphs as you like.
*)

let reference =
  string "[^" *> take_while1 (fun c -> c <> ']' && non_eol c) <* char ']'

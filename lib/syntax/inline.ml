open Angstrom
open Parsers

(* emphasis *)
let delims =
  [ ('*', `Bold)
  ; ('_', `Underline)
  ; ('/', `Italic)
  ; ('+', `Strike_through)
  ; ('~', `Verbatim)
  ; ('=', `Verbatim) ]

let emphasis_token c =
  take_while (function
    | x when x = c -> false
    | '\r' | '\n' -> false
    | _ -> true )

let between_char c = between_char (emphasis_token c) c

let bold = between_char '*' >>= fun s -> return (`Bold s) <?> "Inline bold"

let underline =
  between_char '_' >>= fun s -> return (`Underline s) <?> "Inline underline"

let italic =
  between_char '/' >>= fun s -> return (`Italic s) <?> "Inline italic"

let strike_through =
  between_char '+'
  >>= fun s -> return (`Strike_through s) <?> "Inline strike_through"

(* '=', '~' verbatim *)
let verbatim =
  between_char '=' <|> between_char '~'
  >>= fun s -> return (`Verbatim s) <?> "Inline verbatim"

let emphasis =
  bold <|> underline <|> italic <|> strike_through <?> "Inline emphasis"

let blank = ws >>= fun s -> return (`Blank s)

let token =
  take_while1 (function x -> not @@ List.mem_assoc x delims)
  >>= fun s -> return (`Token s) <?> "Token"

(* run through parsers *)
let inline = many (verbatim <|> emphasis <|> blank <|> token)

(*
parse_string inline "some text *bold* _underline_ /italic/ +strike+ ~code~ other text";;
*)

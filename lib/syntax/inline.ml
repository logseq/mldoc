open Angstrom
open Parsers

type emphasis = [`Bold | `Italic | `Underline | `Strike_through] * t list

and t =
  | Emphasis of emphasis
  (* | Entity of entity *)
  (* | Export_Snippet of export_snippet *)
  (* | Footnote_Reference of footnote_reference *)
  (* | Inline_Call of inline_call *)
  (* | Inline_Source_Block of inline_source_block *)
  (* | Latex_Fragment of latex_fragment *)
  | Break_Lines of int
  (* lines count *)
  (* | Link of link *)
  | Macro of string * string list
  | Radio_Target of string
  | Target of string
  | Subscript of t list
  | Superscript of t list
  | Verbatim of string
  (* | Cookie of stats_cookie *)
  (* | Timestamp of timestamp *)
  | List of t list
  | Plain of string

(* emphasis *)
let delims =
  [ ('*', ('*', `Bold))
  ; ('_', ('_', `Underline))
  ; ('/', ('/', `Italic))
  ; ('+', ('+', `Strike_through))
  ; ('~', ('~', `Verbatim))
  ; ('=', ('=', `Verbatim))
  ; ('[', (']', `Bracket))
  ; ('<', ('>', `Chev))
  ; ('{', ('}', `Brace))
  ; ('(', (')', `Paren)) ]

let emphasis_token c =
  take_while (function
    | x when x = c -> false
    | '\r' | '\n' -> false
    | _ -> true )

let between_char c = between_char (emphasis_token c) c

let bold =
  between_char '*'
  >>= fun s -> return (Emphasis (`Bold, [Plain s])) <?> "Inline bold"

let underline =
  between_char '_'
  >>= fun s -> return (Emphasis (`Underline, [Plain s])) <?> "Inline underline"

let italic =
  between_char '/'
  >>= fun s -> return (Emphasis (`Italic, [Plain s])) <?> "Inline italic"

let strike_through =
  between_char '+'
  >>= fun s ->
  return (Emphasis (`Strike_through, [Plain s])) <?> "Inline strike_through"

(* '=', '~' verbatim *)
let verbatim =
  between_char '=' <|> between_char '~'
  >>= fun s -> return (Verbatim s) <?> "Inline verbatim"

let emphasis =
  choice [bold; underline; italic; strike_through] <?> "Inline emphasis"

let blanks = ws >>= fun s -> return (Plain s)

let concat_tokens t1 t2 = t1 ^ t2

let token =
  take_while1 (fun c -> (not (is_eol c)) && not (List.mem_assoc c delims))
  >>= fun s -> print_endline s ; return (Plain s)

let breaklines = eols >>= fun s -> return (Break_Lines (String.length s))

(* run through parsers *)
let inline = many @@ choice [verbatim; emphasis; blanks; breaklines; token]

(* TODO: *)
(* Case 1: Delims not matched
   "*foo"

   Case 2: Nested emphasis
   "*/foo/*"
*)

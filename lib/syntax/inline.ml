open Angstrom
open Parsers
open Bigstringaf

type emphasis = [`Bold | `Italic | `Underline | `Strike_through] * t list

and t =
  | Emphasis of emphasis
  | Break_Lines of int
  | Verbatim of string
  | Code of string
  | Plain of string

(* emphasis *)
let delims =
  [ ('*', ('*', `Bold))
  ; ('_', ('_', `Underline))
  ; ('/', ('/', `Italic))
  ; ('+', ('+', `Strike_through))
  ; ('~', ('~', `Code))
  ; ('=', ('=', `Verbatim))
  ; ('[', (']', `Bracket))
  ; ('<', ('>', `Chev))
  ; ('{', ('}', `Brace))
  ; ('(', (')', `Paren)) ]

let prev = ref None

let emphasis_token c =
  let blank_before_delimiter = ref false in
  peek_char_fail
  >>= fun x ->
  if is_space x then fail "space before token"
  else
    take_while1 (function
        | x when x = c -> (
            match !prev with
            | Some x ->
              if x = ' ' then blank_before_delimiter := true;
              false
            | None -> false )
        | '\r' | '\n' -> false
        | x ->
          prev := Some x ;
          true )
    >>= (fun s ->
        let blank_before = !blank_before_delimiter in
        blank_before_delimiter := false;
        if blank_before then fail "emphasis_token"
        else return s)

let between c = between_char (emphasis_token c) c
  >>= fun s ->
  peek_char
  >>= fun c ->
  match c with
  | None -> return s
  | Some c ->
    match c with
    | '\n' | '\r' | ' ' | '\t' -> return s
    | _ -> fail "between"

let bold =
  between '*'
  >>= fun s -> return (Emphasis (`Bold, [Plain s])) <?> "Inline bold"

let underline =
  between '_'
  >>= fun s -> return (Emphasis (`Underline, [Plain s])) <?> "Inline underline"

let italic =
  between '/'
  >>= fun s -> return (Emphasis (`Italic, [Plain s])) <?> "Inline italic"

let strike_through =
  between '+'
  >>= fun s ->
  return (Emphasis (`Strike_through, [Plain s])) <?> "Inline strike_through"

(* '=', '~' verbatim *)
let verbatim =
  between '='
  >>= fun s -> return (Verbatim s) <?> "Inline verbatim"

let code =
  between '~'
  >>= fun s -> return (Code s) <?> "Inline code"

let emphasis_choice =
  choice [bold; underline; italic; strike_through] <?> "Inline emphasis"

(* /*hello* +great+/
   Emphasis (Underline, [Emphasis (Bold [Plain hello]), Emphasis (Strike_through [great])])

*)

let non_emphasis =
  take_while1 (function
      | '*' | '_' | '/' | '+' -> false
      | _ -> true)
  >>= fun s -> return (Plain s) <?> "Non emphasis"

let nested_emphasis =
  many @@ choice [emphasis_choice; non_emphasis]

let emphasis =
  emphasis_choice >>=
  function
  | Emphasis (typ, [Plain s]) as e ->
    return e
  | _ -> fail "emphasis don't know how to handle it"


let blanks = ws >>= fun s -> return (Plain s)

let breaklines = eols >>= fun s -> return (Break_Lines (String.length s))

type delimited = [`Non_delimited | `Delimited]

let string_buf = Buffer.create 8

let token =
  scan_state (false, false, None) (fun state c ->
      (*     (start?, delimited?, Option (open_char, close_char, type)) *)
      match c with
      | '\r' | '\n' -> None
      | c -> (
          match state with
          | false, false, _ -> (
              Buffer.add_char string_buf c ;
              match List.assoc_opt c delims with
              | Some (close, typ) -> Some (true, true, Some (c, close, typ))
              | None -> Some (true, false, None) )
          | true, false, _ -> (
              match List.assoc_opt c delims with
              | Some (close, typ) -> None
              | None ->
                Buffer.add_char string_buf c ;
                Some (true, false, None) )
          | (true, true, Some (c', close, typ)) as state ->
            if c <> c' && List.mem_assoc c delims then None
            (* another delimiter *)
            else if c = close then None (* close *)
            else (
              Buffer.add_char string_buf c ;
              Some state )
          | _ -> None ) )
  >>= fun _ ->
  let s = Buffer.contents string_buf in
  Buffer.clear string_buf ; return @@ Plain s

(* TODO: `many` and `choice` together may affect performance, benchmarks are needed *)
let inline = many @@ choice [verbatim; code; blanks; breaklines; emphasis; token]

(* TODO: *)
(* Case 1: DONE
   "world *great*"

   Case 2: DONE
   "*great *"

   Case 3: Nested
   a. "*/foo/*"

   b. "/*_foo_*/"

*)

(*
   open Org_parser__Inline;;
   parse_string inline "* hello*";;
*)

(*
   let s = "*/hello/*"
   let s2 = "+/_*hello*_/+"
 *)

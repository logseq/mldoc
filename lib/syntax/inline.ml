open Angstrom
open Parsers
open Bigstringaf

type emphasis = [`Bold | `Italic | `Underline | `Strike_through] * t list

and url = File of string | Search of string | Complex of complex
and complex = {protocol: string; link: string}
and link = {url: url; label: t list}

and t =
  | Emphasis of emphasis
  | Break_Lines of int
  | Verbatim of string
  | Code of string
  | Plain of string
  | Link of link
  | Target of string

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

let link_delims = ['['; ']'; '<'; '>'; '{'; '}'; '('; ')'; '*'; '$']

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
  let _ = match c with None -> print_endline "" | Some c -> print_char c in
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

let emphasis =
  choice [bold; underline; italic; strike_through] <?> "Inline emphasis"

let blanks = ws >>= fun s -> return (Plain s)

let breaklines = eols >>= fun s -> return (Break_Lines (String.length s))

(* link *)
(* 1. [[url][label]] *)
(* 2. [[search]] *)
let link =
  let url_part = (string "[[" *>
                  take_while (fun c -> c <> ']')
                  <* optional (string "][")) in
  let label_part = (take_while (fun c -> c <> ']')
                    <* string "]]") in
  lift2 (fun url label ->
      let url = if label = "" then
          Search url
        else if url.[0] = '/' || url.[0] = '.' then
          File url
        else
          try
            Scanf.sscanf url "%[^:]:%[^\n]" (fun protocol link ->
                Complex {protocol; link} )
          with _ -> Search url in
      Link {label = [Plain label]; url}
    )
    url_part label_part

(* complex link *)
(* :// *)
let link_inline =
  let protocol_part = (take_while1 is_letter
                       <* string "://") in
  let link_part = (take_while1 (fun c ->
      non_space c && (List.for_all (fun c' -> c <> c') link_delims))) in
  lift2 (fun protocol link ->
      Link
        { label= [Plain (protocol ^ "://" ^ link)]
        ; url= Complex {protocol; link= "//" ^ link} } )
    protocol_part link_part

let target =
  between_string
    "<<"
    (take_while1 (function
         | '>' | '\r' | '\n' -> false
         | _ -> true)
     >>=
     (fun s ->
        print_endline s;
        return @@ Target s))
    ">>"

let plain =
  take_while1 non_space_eol >>= (fun s -> return (Plain s))

(* TODO: configurable *)
let inline_choices =
  choice [link; link_inline; target; verbatim; code; blanks; breaklines; emphasis; plain]

let inline =
  fix (fun inline ->
      let nested_inline = function
        | Emphasis (typ, [Plain s]) ->
          (match parse_string inline s with
             Ok result -> Emphasis (typ, result)
           | Error error -> Emphasis (typ, [Plain s])
          )
        (* TODO: footnote *)
        | Link {label = [Plain s]; url} ->
          (match parse_string inline s with
             Ok result -> Link {label = result; url}
           | Error error -> Link {label = [Plain s]; url}
          )
        | e -> e
      in
      many inline_choices
      >>= fun l ->
      return @@ List.map nested_inline l
    )

(* TODO: `many` and `choice` together may affect performance, benchmarks are needed *)

(*
   parse_string inline "* hello*";;
*)

(*
   let s = "*/hello/*"
   let s2 = "+/_*hello*_/+"
 *)

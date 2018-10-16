open Angstrom
open Parsers
open Bigstringaf

(* TODO:
   1. Performance:
   `many` and `choice` together may affect performance, benchmarks are needed

   2. Security:
   unescape

*)

module Macro = struct
  type t = { name: string;
             arguments: string list}
end

type emphasis = [`Bold | `Italic | `Underline | `Strike_through] * t list

and footnote_reference = {name: string; definition: t list option}

and url = File of string | Search of string | Complex of complex

and complex = {protocol: string; link: string}

and link = {url: url; label: t list}

(** {2 Cookies} *)

(** Cookies are a way to indicate the progress of a task.
    They can be of two form : percentage or absolute value *)
and stats_cookie =
  | Percent of int
  | Absolute of int * int  (** current, max *)

and latex_fragment = Inline of string | Displayed of string

and t =
  | Emphasis of emphasis
  | Break_Lines of int
  | Verbatim of string
  | Code of string
  | Plain of string
  | Link of link
  | Target of string
  | Subscript of string
  | Superscript of string
  | Footnote_Reference of footnote_reference
  | Cookie of stats_cookie
  | Latex_Fragment of latex_fragment
  | Macro of Macro.t

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
              if x = ' ' then blank_before_delimiter := true ;
              false
            | None -> false )
        | '\r' | '\n' -> false
        | x ->
          prev := Some x ;
          true )
    >>= fun s ->
    let blank_before = !blank_before_delimiter in
    blank_before_delimiter := false ;
    if blank_before then fail "emphasis_token" else return s

let between c =
  between_char c c (emphasis_token c)
  >>= fun s ->
  peek_char
  >>= function
  | None -> return s
  | Some c -> (
      match c with '\n' | '\r' | ' ' | '\t' -> return s | _ -> fail "between" )

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
  between '=' >>= fun s -> return (Verbatim s) <?> "Inline verbatim"

let code = between '~' >>= fun s -> return (Code s) <?> "Inline code"

let emphasis =
  choice [bold; underline; italic; strike_through] <?> "Inline emphasis"

let blanks = ws >>= fun s -> return (Plain s)

let breaklines = eols >>= fun s -> return (Break_Lines (String.length s))

(* link *)
(* 1. [[url][label]] *)
(* 2. [[search]] *)
let link =
  let url_part =
    string "[[" *> take_while (fun c -> c <> ']') <* optional (string "][")
  in
  let label_part = take_while (fun c -> c <> ']') <* string "]]" in
  lift2
    (fun url label ->
       let url =
         if label = "" then Search url
         else if url.[0] = '/' || url.[0] = '.' then File url
         else
           try
             Scanf.sscanf url "%[^:]:%[^\n]" (fun protocol link ->
                 Complex {protocol; link} )
           with _ -> Search url
       in
       Link {label= [Plain label]; url} )
    url_part label_part

(* complex link *)
(* :// *)
let link_inline =
  let protocol_part = take_while1 is_letter <* string "://" in
  let link_part =
    take_while1 (fun c ->
        non_space c && List.for_all (fun c' -> c <> c') link_delims )
  in
  lift2
    (fun protocol link ->
       Link
         { label= [Plain (protocol ^ "://" ^ link)]
         ; url= Complex {protocol; link= "//" ^ link} } )
    protocol_part link_part

let target =
  between_string "<<" ">>"
    ( take_while1 (function '>' | '\r' | '\n' -> false | _ -> true)
      >>= fun s -> return @@ Target s )

let plain = take_while1 non_space_eol >>= fun s -> return (Plain s)

(* foo_{bar}, foo^{bar} *)
let subscript, superscript =
  let gen s f =
    string s *> take_while1 (fun c -> non_space c && c <> '}')
    <* char '}' >>| f
  in
  ( gen "_{" (fun x -> print_endline x ; Subscript x)
  , gen "^{" (fun x -> Superscript x) )

(* 1. fn:name *)
(* 2. fn::latex_inline_definition, ignore now *)
(* 3. fn:name:description *)
let id = ref 0

let footnote_reference =
  let name_part =
    string "fn:" *> take_while (fun c -> c <> ':' && non_eol c)
    <* optional (char ':')
  in
  let definition_part = take_while non_space in
  lift2
    (fun name definition ->
       let name =
         if name = "" then (
           incr id ;
           "_anon_" ^ string_of_int !id )
         else name
       in
       if definition = "" then Footnote_Reference {name; definition= None}
       else Footnote_Reference {name; definition= Some [Plain definition]} )
    name_part definition_part

let statistics_cookie =
  between_char '[' ']'
    (take_while1 (fun c ->
         if c = '/' || c = '%' || is_digit c then true else false ))
  >>= fun s ->
  let cookie =
    try Scanf.sscanf s "%d/%d" (fun n n' -> Absolute (n, n')) with _ ->
      Scanf.sscanf s "%d%%" (fun n -> Percent n)
  in
  return (Cookie cookie)

(*
   1. $content$, TeX delimiters for inline math.
   2. \( content \), LaTeX delimiters for inline math.
   3. $$content$$, TeX delimiters for displayed math.
   4. \[ content \], LaTeX delimiters for displayed math.

   If $a^2=b$ and \( b=2 \), then the solution must be
   either $$ a=+\sqrt{2} $$ or \[ a=-\sqrt{2} \].

*)
(*
   latex block.

   \begin{equation}
   x=\sqrt{b}
   \end{equation}

*)
let latex_fragment =
  any_char
  >>= function
  | '$' ->
    any_char
    >>= fun c ->
    if c == '$' then
      (* displayed math *)
      take_while1 (fun x -> x <> '$')
      <* string "$$"
      >>| fun s -> Latex_Fragment (Displayed s)
    else
      (* inline math *)
      take_while1 (fun x -> x <> '$')
      <* char '$'
      >>| fun s -> Latex_Fragment (Inline s)
  | '\\' -> (
      any_char
      >>= function
      | '[' ->
        (* displayed math *)
        end_string "\\]" (fun s -> Latex_Fragment (Displayed s))
      | '(' ->
        (* inline math *)
        end_string "\\)" (fun s -> Latex_Fragment (Inline s))
      | _ -> fail "latex fragment \\" )
  | _ -> fail "latex fragment"

(*
   Define: #+MACRO: demo =$1= ($1)
   Usage:  {{{demo(arg1, arg2, ..., argn)}}}
*)
let macro =
  lift2 (fun name arguments ->
      let arguments = String.split_on_char ',' arguments in
      let arguments = List.map String.trim arguments in
      Macro { name; arguments})
    (string "{{{" *>
     take_while1 (fun c -> c <> '(')
     <* char '(')
    (take_while1 (fun c -> c <> ')')
     <* string ")}}}")

let timestamp () = failwith "unimplemented!"

let entity () = failwith "unimplemented!"

(* TODO: configurable *)
let inline_choices =
  choice
    [ latex_fragment
    ; statistics_cookie
    ; footnote_reference
    ; link
    ; link_inline
    ; target
    ; verbatim
    ; code
    ; blanks
    ; breaklines
    ; emphasis
    ; subscript
    ; superscript
    ; plain ]

let inline =
  fix (fun inline ->
      let nested_inline = function
        | Emphasis (typ, [Plain s]) -> (
            match parse_string inline s with
            | Ok result -> Emphasis (typ, result)
            | Error error -> Emphasis (typ, [Plain s]) )
        | Link {label= [Plain s]; url} -> (
            match parse_string inline s with
            | Ok result -> Link {label= result; url}
            | Error error -> Link {label= [Plain s]; url} )
        | Footnote_Reference {definition; name} as f -> (
            match definition with
            | None -> f
            | Some [Plain s] -> (
                match parse_string inline s with
                | Ok result -> Footnote_Reference {definition= Some result; name}
                | Error error -> f )
            | _ -> failwith "definition" )
        | e -> e
      in
      many inline_choices >>= fun l -> return @@ List.map nested_inline l )

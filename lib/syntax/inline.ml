open Angstrom
open Parsers
open Prelude
open Conf

(* TODO:
   1. Performance:
   `many` and `choice` together may affect performance, benchmarks are needed
*)

module Macro = struct
  type t = {name: string; arguments: string list} [@@deriving yojson]
end

type emphasis = [`Bold | `Italic | `Underline | `Strike_through | `Highlight] * t list [@@deriving yojson]

and footnote_reference = {id: int; name: string; definition: t list option} [@@deriving yojson]

and url = File of string | Search of string | Complex of complex [@@deriving yojson]

and complex = {protocol: string; link: string} [@@deriving yojson]

and link = {url: url; label: t list; title: string option} [@@deriving yojson]

(** {2 Cookies} *)

(** Cookies are a way to indicate the progress of a task.
    They can be of two form : percentage or absolute value *)
and stats_cookie =
    Percent of int
  | Absolute of int * int  (** current, total *)
[@@deriving yojson]

and latex_fragment = Inline of string | Displayed of string [@@deriving yojson]

and clock_item = Started of Timestamp.t | Stopped of Timestamp.range

and timestamp =
    Scheduled of Timestamp.t
  | Deadline of Timestamp.t
  | Date of Timestamp.t
  | Closed of Timestamp.t
  | Clock of clock_item
  | Range of Timestamp.range
[@@deriving yojson]

(** {2 Inline call} *)
and inline_call = {
  program : string; (** The name of the block to call *)
  arguments : (string * string) list; (** The arguments to the block *)
  inside_headers : string option; (** The inside header arguments *)
  end_headers : string option; (** The end header arguments *)
}
(** See org's documentation for more information *)

and inline_source_block = {
  language: string; (** The language of the code block *)
  options: string; (** The options *)
  code: string; (** The code *)
}

and t =
    Emphasis of emphasis
  | Break_Line
  | Hard_Break_Line
  | Paragraph_Sep
  | Verbatim of string
  | Code of string
  | Spaces of string
  | Plain of string
  | Link of link
  | Target of string
  | Subscript of t list
  | Superscript of t list
  | Footnote_Reference of footnote_reference
  | Cookie of stats_cookie
  | Latex_Fragment of latex_fragment
  | Macro of Macro.t
  | Entity of Entity.t
  | Timestamp of timestamp
  | Radio_Target of string
  | Export_Snippet of string * string
  | Inline_Source_Block of inline_source_block
  | Email of Email_address.t
  | Block_reference of string   (** Block reference *)
[@@deriving yojson]

let link_delims = ['['; ']'; '<'; '>'; '{'; '}'; '('; ')'; '*'; '$']

let prev = ref None

let email = Email_address.email >>| fun email -> Email email

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
      match c with
      | '\n' | '\r' | ' ' | '\t' | '.' | ',' | '!' | '?' | '"' | '\'' | ')' | '-' | ':' | ';' | '[' | '}'
        -> return s
      | _ -> fail "between" )

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

(* ^^highlight^^ *)
let highlight =
  between_string "^^" "^^"
    ( take_while1 (fun c ->
          if List.exists (fun d -> c = d) ['^'; '\r'; '\n'] then
            false
          else
            true)
      >>= fun s -> return (Emphasis (`Highlight, [Plain s])))
  <?> "Inline highlight"

(* '=', '~' verbatim *)
let verbatim =
  between '=' >>= fun s -> return (Verbatim s) <?> "Inline verbatim"

let markdown_escape_backticks =
  string "``" *>
  end_string "``" (fun s -> Code s)

let code config =
  let is_markdown = String.equal config.format "Markdown" in
  let c = if is_markdown then '`' else '~' in
  let p = between c >>= fun s -> return (Code s) <?> "Inline code" in
  if is_markdown then p <|> markdown_escape_backticks else p

(* TODO: optimization *)
let org_plain_delims = [' '; '\\'; '_'; '^';]
let markdown_plain_delims = [' '; '\\'; '_'; '^'; '['; '*'; '^'; '~']
(* replace list with a  *)
let in_plain_delims config c =
  let plain_delims = match config.format with
    | "Org" -> org_plain_delims
    | "Markdown" -> markdown_plain_delims
  in
  List.exists (fun d -> c = d) plain_delims

let whitespaces = ws >>= fun spaces -> return (Plain spaces)

let plain config =
  (scan1 false (fun state c ->
       if (non_eol c && not (in_plain_delims config c)) then
         Some true
       else
         None)
   >>= fun (s, _state) ->
   return (Plain s))
  <|>
  (word >>= fun s -> return (Plain s))
  <|>
  (any_char >>= fun c ->
   if (in_plain_delims config c) then
     return (Plain (String.make 1 c))
   else
     fail "plain"
  )

(* Slow version *)
(* let emphasis =
 *   choice [bold; underline; italic; strike_through; highlight] *)
let org_emphasis =
  peek_char_fail >>= function
  | '*' -> bold
  | '_' -> underline
  | '/' -> italic
  | '+' -> strike_through
  | '^' -> highlight
  | _ -> fail "Inline emphasis"

let md_em_parser pattern typ =
  let pattern_c = String.get pattern 0 in
  let stop_chars = [pattern_c; '\r'; '\n'] in
  between_string pattern pattern
    ( take_while1 (fun c ->
          if List.exists (fun d -> c = d) stop_chars then
            false
          else
            true)
      >>= fun s -> return @@ Emphasis (typ, [Plain s]) )

let md_em_nested_parser pattern =
  let pattern_c = String.get pattern 0 in
  let stop_chars = [pattern_c; '\r'; '\n'] in
  between_string pattern pattern
    ( take_while1 (fun c ->
          if List.exists (fun d -> c = d) stop_chars then
            false
          else
            true)
      >>= fun s -> return @@ Emphasis (`Italic, [Emphasis (`Bold, [Plain s])]))

(* TODO: html tags support *)
(* <ins></ins> *)
(* let underline =    *)

let markdown_emphasis =
  peek_char_fail >>= function
  | '*' -> choice [md_em_parser "**" `Bold; md_em_parser "*" `Italic; md_em_nested_parser "***"]
  | '_' -> choice [md_em_parser "__" `Bold; md_em_parser "_" `Italic; md_em_nested_parser "___"]
  | '~' -> md_em_parser "~~" `Strike_through
  | '^' -> md_em_parser "^^" `Highlight
  | _ -> fail "Inline emphasis"

let emphasis config =
  match config.format with
  | "Org" -> org_emphasis
  | "Markdown" -> markdown_emphasis

let org_hard_breakline = string "\\" <* eol
let hard_breakline = choice [org_hard_breakline; Markdown_line_breaks.parse] >>= fun _ -> return Hard_Break_Line
let breakline = eol >>= fun _ -> return Break_Line

let radio_target =
  between_string "<<<" ">>>"
    ( take_while1 (function '>' | '\r' | '\n' -> false | _ -> true)
      >>= fun s -> return @@ Radio_Target s )

let target =
  between_string "<<" ">>"
    ( take_while1 (function '>' | '\r' | '\n' -> false | _ -> true)
      >>= fun s -> return @@ Target s )

(* \alpha *)
let entity =
  char '\\' *> take_while1 is_letter
  >>| fun s ->
  try
    let entity = Entity.find s in
    Entity entity
  with Not_found ->
    Plain s

(* FIXME: nested emphasis not working *)
(* foo_bar, foo_{bar}, foo^bar, foo^{bar} *)
let gen_script config s f =
  let p = many1 (choice [(emphasis config); plain config; whitespaces; entity]) in
  (string (s ^ "{") *> take_while1 (fun c -> non_eol c && c <> '}')
   <* char '}')
  <|>
  (string s *> take_while1 (fun c -> non_space c))
  >>| fun s ->
  match parse_string p s with
  | Ok result -> f result
  | Error _e -> f [Plain s]

let subscript config =
  gen_script config "_" (fun x -> Subscript x)

let superscript config =
  gen_script config "^" (fun x -> Superscript x)

let nested_emphasis config =
  let rec aux_nested_emphasis = function
    | Plain s ->
      Plain s
    | Emphasis (typ, [Plain s]) as e ->
      let parser = (many1 (choice [(emphasis config); (subscript config); (superscript config); plain config])) in
      (match parse_string parser s with
       | Ok [Plain _] -> e
       | Ok result -> Emphasis (typ,
                                List.map aux_nested_emphasis result)
       | Error _error -> e)
    | Emphasis (`Italic, [Emphasis (`Bold, _)]) as e ->
      e
    | Subscript _ as s ->
      s
    | Superscript _ as s ->
      s
    | _ ->
      failwith "nested_emphasis" in
  (emphasis config) >>= fun e ->
  return (aux_nested_emphasis e)

let statistics_cookie =
  between_char '[' ']'
    (take_while1 (fun c ->
         if c = '/' || c = '%' || is_digit c then true else false ))
  >>= fun s ->
  try let cookie = Scanf.sscanf s "%d/%d" (fun n n' -> Absolute (n, n')) in
    return (Cookie cookie)
  with _ ->
  try let cookie = Scanf.sscanf s "%d%%" (fun n -> Percent n) in
    return (Cookie cookie)
  with _ ->
    fail "statistics_cookie"

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
let latex_fragment config =
  any_char
  >>= function
  | '$' ->
    any_char
    >>= fun c ->
    if c == '$' then
      (* displayed math *)
      take_while1 (fun x -> x <> '$' && x <> '\r' && x <> '\n')
      <* string "$$"
      >>| fun s -> Latex_Fragment (Displayed s)
    else
      (* inline math *)
      take_while1 (fun x -> x <> '$' && x <> '\r' && x <> '\n')
      <* char '$'
      >>| fun s -> Latex_Fragment (Inline (Char.escaped c ^ s))
  | '\\' ->
    (
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
  lift2
    (fun name arguments ->
       let arguments = String.split_on_char ',' arguments in
       let arguments = List.map String.trim arguments in
       Macro {name; arguments} )
    (string "{{{" *> take_while1 (fun c -> c <> '(') <* char '(')
    (take_while1 (fun c -> c <> ')') <* string ")}}}")

let date_time close_char ~active typ =
  let open Timestamp in
  let space = satisfy is_space in
  let non_spaces = take_while1 (fun c -> non_space c && c <> close_char) in
  let date_parser = non_spaces <* space >>| fun s -> parse_date s in
  let day_name_parser = letters in
  (* time_or_repeat_1 *)
  let tr1_parser = optional (space *> non_spaces) in
  (* time_or_repeat_2 *)
  let tr2_parser = optional (space *> non_spaces) in
  date_parser >>= function
  | None -> fail "date parser"
  | Some date ->
    lift3
      (fun wday time_or_repeat tr2 ->
         let date, time, repetition =
           match time_or_repeat with
           | None -> (date, None, None)
           | Some s -> (
               match tr2 with
               | None -> (
                   match s.[0] with
                   | ('+' | '.') as c -> (* repeat *)
                     repetition_parser s date None c
                   | _ ->
                     (* time *)
                     let time = parse_time s in
                     (date, time, None) )
               | Some s' ->
                 let time = parse_time s in
                 repetition_parser s' date time s'.[0] )
         in
         match typ with
         | "Scheduled" -> Timestamp (Scheduled {date; wday; time; repetition; active})
         | "Deadline" -> Timestamp (Deadline {date; wday; time; repetition; active})
         | "Closed" -> Timestamp (Closed {date; wday; time; repetition; active})
         | "Clock" -> Timestamp (Clock (Started {date; wday; time; repetition; active}))
         | _ -> Timestamp (Date {date; wday; time; repetition; active}) )
      day_name_parser tr1_parser tr2_parser
    <* char close_char

(* DEADLINE: <2018-10-16 Tue>
   DEADLINE: <2008-02-10 Sun +1w>
   DEADLINE: <2008-02-10 Sun ++1w> (* still Sunday, forget old ones *)
   DEADLINE: <2005-11-01 Tue .+1m> (* from today, not exactly Tuesday *)
   <2018-10-16 Tue 21:20>
   <2007-05-16 Wed 12:30 +1w>

   Not supported:
   range_1: 2006-11-02 Thu 20:00-22:00
*)

let general_timestamp =
  let active_parser typ = date_time '>' ~active:true typ in
  let closed_parser typ = date_time ']' ~active:false typ in
  let parse rest typ =
    (* scheduled *)
    string rest *> ws *> any_char
    >>= function
    | '<' -> active_parser typ
    | '[' -> closed_parser typ
    | _ -> fail "general_timestamp"
  in
  spaces *> any_char
  >>= function
  | '<' -> active_parser "Date"
  | '[' -> closed_parser "Date"
  | 'S' -> parse "CHEDULED:" "Scheduled"
  | 'C' -> (
      Angstrom.take 3
      >>= function
      | "LOS" -> parse "ED:" "Closed"
      | "LOC" -> parse "K:" "Clock"
      | _ -> fail "general_timestamp C" )
  | 'D' -> parse "EADLINE:" "Deadline"
  | _ -> fail "general_timestamp"

(* example: <2004-08-23 Mon>--<2004-08-26 Thu> *)
(* clock:
 *** Clock Started
     CLOCK: [2018-09-25 Tue 13:49]

 *** DONE Clock stopped
    CLOSED: [2018-09-25 Tue 13:51]
    CLOCK: [2018-09-25 Tue 13:50] *)
let range =
  let extract_time t =
    match t with
    | Timestamp t -> (
        match t with
        | Date t | Scheduled t | Deadline t | Closed t -> t
        | _ -> failwith "illegal timestamp" )
    | _ -> failwith "illegal timestamp"
  in
  lift3
    (fun clock t1 t2 ->
       let t1 = extract_time t1 in
       let t2 = extract_time t2 in
       if clock = "CLOCK:" then
         Timestamp (Clock (Stopped {start= t1; stop= t2}))
       else
         Timestamp (Range {start= t1; stop= t2}))
    (spaces *> string "CLOCK:" <* spaces)
    (general_timestamp <* string "--")
    general_timestamp

let timestamp =
  range <|> general_timestamp

(* complex link, or auto link *)
(* :// *)
let link_inline _config =
  let protocol_part = take_while1 is_letter <* string "://" in
  let link_part =
    take_while1 (fun c ->
        non_space c && List.for_all (fun c' -> c <> c') link_delims )
  in
  let p = lift2
      (fun protocol link ->
         Link
           { label= [Plain (protocol ^ "://" ^ link)]
           ; url= Complex {protocol; link= "//" ^ link}
           ; title= None} )
      protocol_part link_part in
  p <|> (between_char '<' '>' p)

(* Build direct links *)
let concat_plains config inlines =
  let l = List.fold_left (fun acc inline ->
      match inline with
      | Plain s ->
        (match acc with
         | [] -> [Plain s]
         | (Plain s') :: tl ->
           if starts_with s "//" then (* might be a direct link *)
             let (l, r) = splitr non_space s' in
             let (l', r') = splitl non_space s in
             let link = r ^ l' in
             match parse_string (link_inline config) link with
             | Ok result ->
               (Plain r') :: result :: (Plain l) :: tl
             | Error _e ->
               (Plain (s' ^ s)) :: tl
           else
             (Plain (s' ^ s)) :: tl
         | _ ->
           Plain s :: acc)
      | other -> other :: acc
    ) [] inlines in
  List.rev l

(* link *)
(* 1. [label](url)
   2. [label](url "title"), for example:
      My favorite search engine is [Duck Duck Go](https://duckduckgo.com "The best search engine for privacy").
*)
(* TODO: URI encode *)
let markdown_link config =
  let label_part = char '[' *> take_while1 (fun c -> c <> ']') <* optional (string "](") in
  let url_part =
    take_while (fun c -> c <> ')') <* string ")"
  in
  lift2
    (fun label url ->
       let (url, title) = split_first '"' url in
       let url =
         if (not (String.equal url "")) && (url.[0] = '/' || url.[0] = '.') then File url
         else
           try
             Scanf.sscanf url "%[^:]:%[^\n]" (fun protocol link ->
                 Complex {protocol; link})
           with _ -> Search url
       in
       let parser = (many1 (choice [(nested_emphasis config); latex_fragment config;
                                    entity; (code config); (subscript config);
                                    (superscript config); plain config; whitespaces])) in
       let label = match parse_string parser label with
           Ok result -> concat_plains config result
         | Error _e -> [Plain label] in
       let title = if String.equal title "" || String.equal title "\"" then
           None
         else
           Some (String.sub title 0 (String.length title - 1))
       in
       Link {label; url; title})
    label_part url_part

(* TODO: make sure it's a proper image format. *)
let markdown_image config =
  let label_part = string "![" *> take_while (fun c -> c <> ']') <* optional (string "](") in
  let url_part =
    take_while (fun c -> c <> ')') <* string ")"
  in
  lift2
    (fun label url ->
       let url =
         if url.[0] = '/' || url.[0] = '.' then File url
         else
           try
             Scanf.sscanf url "%[^:]:%[^\n]" (fun protocol link ->
                 Complex {protocol; link})
           with _ -> Search url
       in
       let parser = (many1 (choice [(nested_emphasis config); latex_fragment config;
                                    entity; (code config); (subscript config);
                                    (superscript config); plain config; whitespaces])) in
       let label = match parse_string parser label with
           Ok result -> concat_plains config result
         | Error _e -> [Plain label] in
       let title = None in
       Link {label; url; title} )
    label_part url_part

(* 1. [[url][label]] *)
(* 2. [[search]] *)
let org_link config =
  let url_part =
    string "[[" *> take_while1 (fun c -> c <> ']') <* optional (string "][")
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
       let parser = (many1 (choice [(nested_emphasis config); latex_fragment config; entity; (code config); (subscript config); (superscript config); plain config; whitespaces])) in
       let label = match parse_string parser label with
           Ok result -> concat_plains config result
         | Error _e -> [Plain label] in
       let title = None in
       Link {label; url; title} )
    url_part label_part

let link config =
  match config.format with
  | "Org" -> org_link config
  | "Markdown" -> markdown_link config <|> org_link config (* page reference *)

let export_snippet =
  let name = take_while1 (fun c -> non_space_eol c && c <> ':') <* char ':' in
  let content = take_while1 (function
      | '@' -> false
      | '\r' | '\n' -> false
      | _ -> true
    ) in
  between_string "@@" "@@"
    (lift2 (fun name content ->
         Export_Snippet (name, content))
        name content)

(* src_LANG[headers]{your code} *)
let inline_source_code =
  let language = take_while1 (fun c -> is_letter c || is_digit c) in
  let options = between_char '[' ']' (take_while1 (fun c -> non_eol c && c <> ']')) in
  let code = between_char '{' '}' (take_while1 (fun c -> non_eol c && c <> '}')) in
  string "src_" *>
  lift3 (fun language options code ->
      Inline_Source_Block { language; options; code })
    language options code

let id = ref 0

let incr_id id =
  incr id;
  !id

let footnote_inline_definition config ?(break = false) definition =
  let choices =
    [(markdown_image config); (link config); email; (link_inline config); radio_target; target; latex_fragment config; (nested_emphasis config); entity;
     (code config); (subscript config); (superscript config); plain config; whitespaces] in
  let parser = (many1 (choice choices)) in
  match parse_string parser definition with
  | Ok result ->
    let result = concat_plains config result in
    result
  | Error _e ->
    [Plain definition]

let latex_footnote config =
  string "[fn::" *> take_while1 (fun c -> c <> ']' && non_eol c)
  <* char ']' >>| fun definition ->
  Footnote_Reference {id = incr_id id; name = ""; definition= Some (footnote_inline_definition config definition)}

let markdown_footnote_reference =
  Markdown_footnote.reference
  >>= fun name ->
  return (Footnote_Reference {id = incr_id id; name; definition= None})

let org_inline_footnote_or_reference config =
  latex_footnote config
  <|>
  let name_part =
    string "[fn:" *> take_while1 (fun c -> c <> ':' && c <> ']' && non_eol c)
    <* optional (char ':') in
  let definition_part = take_while (fun c -> c <> ']' && non_eol c) <* char ']' in
  lift2
    (fun name definition ->
       let name =
         if name = "" then (
           incr id ;
           "_anon_" ^ string_of_int !id )
         else name
       in
       if definition = "" then Footnote_Reference {id = incr_id id; name; definition= None}
       else Footnote_Reference {id = incr_id id; name; definition= Some (footnote_inline_definition config definition)} )
    name_part definition_part

let inline_footnote_or_reference config =
  match config.format with
  | "Org" -> org_inline_footnote_or_reference config
  | "Markdown" -> markdown_footnote_reference

let break_or_line =
  let line = line >>= fun s -> return (Plain s) in
  choice [line; hard_breakline; breakline]
(* choice [line; breakline; allow_breakline] *)

(* TODO: Allow to disable this *)
let block_reference _config =
  Block_reference.parse
  >>= fun s -> return @@ Block_reference s

(* TODO: configurable, re-order *)
let inline_choices config =
  let choices =
    [
      latex_fragment config            (* '$' '\' *)
    ; inline_footnote_or_reference config        (* 'f', fn *)
    ; block_reference config               (* (()) *)
    ; hard_breakline            (* "\\" *)
    ; breakline                 (* '\n' *)
    ; timestamp                 (* '<' '[' 'S' 'C' 'D'*)
    ; entity                    (* '\' *)
    ; macro                     (* '{' *)
    ; statistics_cookie         (* '[' *)
    ; markdown_image config
    ; link config                      (* '[' [[]] *)
    ; link_inline config               (*  *)
    ; email
    ; export_snippet
    ; radio_target              (* "<<<" *)
    ; target                    (* "<" *)
    ; verbatim                  (*  *)
    ; code config                      (* '=' *)
    ; nested_emphasis config
    ; subscript config                 (* '_' "_{" *)
    ; superscript config               (* '^' "^{" *)
    ; plain config
    ] in
  choice choices

let parse config =
  (many1 (inline_choices config) >>| fun l ->
   concat_plains config l)
  <?> "inline"

let string_of_url = function
  | File s | Search s -> s
  | Complex {link; protocol= "file"} -> link
  | Complex {link; protocol} -> protocol ^ ":" ^ link

let rec ascii = function
  | Footnote_Reference ref -> Option.map_default asciis "" ref.definition
  | Link l -> asciis l.label
  | Emphasis (_, t) -> asciis t
  | Subscript l | Superscript l -> asciis l
  | Latex_Fragment (Inline s) | Plain s | Verbatim s -> s
  | Entity e -> e.Entity.unicode
  | _ -> ""

and asciis l = String.concat "" (List.map ascii l)

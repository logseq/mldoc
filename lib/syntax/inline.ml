open Angstrom
open Parsers
open Prelude
open Conf

(* TODO:
   1. Performance:
   `many` and `choice` together may affect performance, benchmarks are needed
*)

module Macro = struct
  type t =
    { name : string
    ; arguments : string list
    }
  [@@deriving yojson]
end

type emphasis =
  [ `Bold | `Italic | `Underline | `Strike_through | `Highlight ] * t list
[@@deriving yojson]

and footnote_reference =
  { id : int
  ; name : string
  ; definition : t list option
  }
[@@deriving yojson]

and url =
  | File of string
  | Search of string
  | Complex of complex
[@@deriving yojson]

and complex =
  { protocol : string
  ; link : string
  }
[@@deriving yojson]

and link =
  { url : url
  ; label : t list
  ; title : string option
  ; full_text : string
  ; metadata : string
  }
[@@deriving yojson]

(** {2 Cookies} *)

(** Cookies are a way to indicate the progress of a task.
    They can be of two form : percentage or absolute value *)
and stats_cookie =
  | Percent of int
  | Absolute of int * int  (** current, total *)
[@@deriving yojson]

and latex_fragment =
  | Inline of string
  | Displayed of string
[@@deriving yojson]

and clock_item =
  | Started of Timestamp.t
  | Stopped of Range.t

and timestamp =
  | Scheduled of Timestamp.t
  | Deadline of Timestamp.t
  | Date of Timestamp.t
  | Closed of Timestamp.t
  | Clock of clock_item
  | Range of Range.t
[@@deriving yojson]

(** {2 Inline call} *)
and inline_call =
  { program : string  (** The name of the block to call *)
  ; arguments : (string * string) list  (** The arguments to the block *)
  ; inside_headers : string option  (** The inside header arguments *)
  ; end_headers : string option  (** The end header arguments *)
  }
(** See org's documentation for more information *)

and inline_source_block =
  { language : string  (** The language of the code block *)
  ; options : string  (** The options *)
  ; code : string  (** The code *)
  }

and t =
  | Emphasis of emphasis
  | Break_Line
  | Hard_Break_Line
  | Verbatim of string
  | Code of string
  | Tag of string
  | Spaces of string
  | Plain of string
  | Link of link
  | Nested_link of Nested_link.t
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
  | Block_reference of string  (** Block reference *)
  | Inline_Hiccup of string
  | Inline_Html of string
[@@deriving yojson]

let quicklink_delims = [ '>' ] @ eol_chars

let inline_link_delims = [ '['; ']'; '<'; '>'; '{'; '}'; '('; ')' ] @ eol_chars

let email = Email_address.email >>| fun email -> Email email

let between ?(e = None) s =
  let end_s =
    match e with
    | None -> s
    | Some s -> s
  in
  let c = end_s.[0] in
  let not_match_chars = [ c; '\r'; '\n' ] in
  between_string s end_s
    (take_while1 (fun c ->
         if List.exists (fun d -> c = d) not_match_chars then
           false
         else
           true))

let bold =
  between "*"
  >>= (fun s -> return (Emphasis (`Bold, [ Plain s ])))
  <?> "Inline bold"

let underline =
  between "_"
  >>= (fun s -> return (Emphasis (`Underline, [ Plain s ])))
  <?> "Inline underline"

let italic =
  between "/"
  >>= (fun s -> return (Emphasis (`Italic, [ Plain s ])))
  <?> "Inline italic"

let strike_through =
  between "+"
  >>= (fun s -> return (Emphasis (`Strike_through, [ Plain s ])))
  <?> "Inline strike_through"

(* ^^highlight^^ *)
let highlight =
  between "^^"
  >>= (fun s -> return (Emphasis (`Highlight, [ Plain s ])))
  <?> "Inline highlight"

(* '=', '~' verbatim *)
let verbatim =
  between "=" >>= (fun s -> return (Verbatim s)) <?> "Inline verbatim"

let markdown_escape_backticks = string "``" *> end_string "``" (fun s -> Code s)

let code_aux_p c =
  between c
  >>= (fun s ->
        if String.length s > 0 then
          return (Code s)
        else
          fail "Empty code")
  <?> "Inline code"

let org_code = code_aux_p "~"

let md_code = code_aux_p "`" <|> markdown_escape_backticks

let code config =
  match config.format with
  | Org -> org_code
  | Markdown -> md_code

(* TODO: optimization *)
let org_plain_delims = [ ' '; '\\'; '_'; '^'; '['; '$' ]

let markdown_plain_delims = [ ' '; '\\'; '_'; '^'; '['; '*'; '~'; '`'; '$' ]

(* replace list with a  *)
let in_plain_delims config c =
  let plain_delims =
    match config.format with
    | Org -> org_plain_delims
    | Markdown -> markdown_plain_delims
  in
  List.mem c plain_delims

let whitespaces = ws >>| fun spaces -> Plain spaces

let plain config =
  take_while1 (fun c -> non_eol c && not (in_plain_delims config c))
  >>| (fun s -> Plain s)
  <|> (ws >>| fun s -> Plain s)
  <|> ( char '\\' *> satisfy non_tab_or_space >>| fun c ->
        Plain (String.make 1 c) )
  <|> ( any_char >>= fun c ->
        if in_plain_delims config c then
          return (Plain (String.make 1 c))
        else
          fail "plain" )

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

let concat_plains l =
  List.fold_left
    (fun r e ->
      match (r, e) with
      | Plain h :: t, Plain e -> Plain (h ^ e) :: t
      | _ -> e :: r)
    [] l
  |> List.rev

let md_em_parser pattern typ =
  let pattern_c = pattern.[0] in
  let stop_chars = [ pattern_c; '\r'; '\n' ] in
  (* inline code has higher precedence than any other inline constructs
     except HTML tags and autolinks *)
  let stop_chars_include_inline_code_delim = '`' :: stop_chars in
  between_string pattern pattern
  @@ many1
  @@ choice
       [ ( take_while1 (fun c ->
               not @@ List.mem c stop_chars_include_inline_code_delim)
         >>| fun s -> Plain s )
       ; md_code
       ; ( take_while1 (fun c -> not @@ List.mem c stop_chars) >>| fun s ->
           Plain s )
       ]
  >>| fun l -> Emphasis (typ, concat_plains l)

let md_em_nested_parser pattern =
  let pattern_c = pattern.[0] in
  let stop_chars = [ pattern_c; '\r'; '\n' ] in
  (* inline code has higher precedence than any other inline constructs
     except HTML tags and autolinks *)
  let stop_chars_include_inline_code_delim = '`' :: stop_chars in
  between_string pattern pattern
  @@ many1
  @@ choice
       [ ( take_while1 (fun c ->
               not @@ List.mem c stop_chars_include_inline_code_delim)
         >>| fun s -> Plain s )
       ; md_code
       ; ( take_while1 (fun c -> not @@ List.mem c stop_chars) >>| fun s ->
           Plain s )
       ]
  >>| fun l -> Emphasis (`Italic, [ Emphasis (`Bold, concat_plains l) ])

(* TODO: html tags support *)
(* <ins></ins> *)
(* let underline =    *)

let markdown_emphasis =
  peek_char_fail >>= function
  | '*' ->
    choice
      [ md_em_parser "**" `Bold
      ; md_em_parser "*" `Italic
      ; md_em_nested_parser "***"
      ]
  | '_' ->
    choice
      [ md_em_parser "__" `Bold
      ; md_em_parser "_" `Italic
      ; md_em_nested_parser "___"
      ]
  | '~' -> md_em_parser "~~" `Strike_through
  | '^' -> md_em_parser "^^" `Highlight
  | _ -> fail "Inline emphasis"

let emphasis config =
  match config.format with
  | Org -> org_emphasis
  | Markdown -> markdown_emphasis

let org_hard_breakline = string "\\" <* eol

let hard_breakline =
  choice [ org_hard_breakline; Markdown_line_breaks.parse ] >>= fun _ ->
  return Hard_Break_Line

let breakline = eol >>= fun _ -> return Break_Line

let radio_target =
  between_string "<<<" ">>>"
    ( take_while1 (function
        | '>'
        | '\r'
        | '\n' ->
          false
        | _ -> true)
    >>= fun s -> return @@ Radio_Target s )

let target =
  between_string "<<" ">>"
    ( take_while1 (function
        | '>'
        | '\r'
        | '\n' ->
          false
        | _ -> true)
    >>= fun s -> return @@ Target s )

(* \alpha *)
let entity =
  char '\\' *> take_while1 is_letter >>| fun s ->
  try
    let entity = Entity.find s in
    Entity entity
  with Not_found -> Plain s

(* FIXME: nested emphasis not working *)
(* foo_bar, foo_{bar}, foo^bar, foo^{bar} *)
let gen_script config s f =
  let is_markdown = Conf.is_markdown config in
  let p1 =
    if is_markdown then
      fail "markdown subscript with only _"
    else
      string s *> take_while1 (fun c -> non_space c)
  in
  let p =
    many1 (choice [ emphasis config; plain config; whitespaces; entity ])
  in
  string (s ^ "{") *> take_while1 (fun c -> non_eol c && c <> '}')
  <* char '}' <|> p1
  >>| fun s ->
  match parse_string ~consume:All p s with
  | Ok result -> f @@ concat_plains result
  | Error _e -> f [ Plain s ]

let subscript config = gen_script config "_" (fun x -> Subscript x)

let superscript config = gen_script config "^" (fun x -> Superscript x)

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
let latex_fragment _config =
  any_char >>= function
  | '$' -> (
    any_char >>= fun c ->
    if c == '$' then
      (* displayed math *)
      take_while (fun x -> x <> '$' && x <> '\r' && x <> '\n') <* string "$$"
      >>| fun s -> Latex_Fragment (Displayed s)
    else if c == ' ' then
      fail "inline math shouldn't start with a space"
    else
      (* inline math *)
      take_while (fun x -> x <> '$' && x <> '\r' && x <> '\n') <* char '$'
      >>= fun s ->
      match last_char s with
      | Some ' ' -> fail "inline math shouldn't end with a space"
      | _ -> return @@ Latex_Fragment (Inline (String.make 1 c ^ s)))
  | '\\' -> (
    any_char >>= function
    | '[' ->
      (* displayed math *)
      end_string "\\]" (fun s -> Latex_Fragment (Displayed s))
    | '(' ->
      (* inline math *)
      end_string "\\)" (fun s -> Latex_Fragment (Inline s))
    | _ -> fail "latex fragment \\")
  | _ -> fail "latex fragment"

(* {:width 200 :height 200} *)
let metadata =
  between ~e:(Some "}") "{"
  >>= (fun s -> return ("{" ^ s ^ "}"))
  <|> string "{}" <|> return ""

let link_inline =
  let protocol_part = take_while1 is_letter_or_digit <* string "://" in
  let before_path_part =
    take_while1 (fun c ->
        non_space c && c <> '/' && c <> '?' && c <> '#'
        && not (List.mem c inline_link_delims))
  in
  let remaining_part =
    (fun c remain -> String.make 1 c ^ remain)
    <$> choice [ char '/'; char '?'; char '#' ]
    <*> string_contains_balanced_brackets
          ~excluded_ending_chars:[ ','; ';'; '.'; '!'; '?' ]
          [ ('(', ')'); ('[', ']') ]
          (('{' :: space_chars) @ eol_chars)
    <|> return ""
  in
  lift4
    (fun protocol before_path remain metadata ->
      Link
        { label = [ Plain (protocol ^ "://" ^ before_path ^ remain) ]
        ; url = Complex { protocol; link = "//" ^ before_path ^ remain }
        ; title = None
        ; full_text = protocol ^ "://" ^ before_path ^ remain ^ metadata
        ; metadata
        })
    protocol_part before_path_part remaining_part metadata

let quick_link_aux ?(delims = inline_link_delims) _config =
  let protocol_part = take_while1 is_letter_or_digit <* string "://" in
  let link_part =
    take_while1 (fun c -> non_space c && not (List.mem c delims))
  in
  lift3
    (fun protocol link metadata ->
      Link
        { label = [ Plain (protocol ^ "://" ^ link) ]
        ; url = Complex { protocol; link = "//" ^ link }
        ; title = None
        ; full_text = protocol ^ "://" ^ link ^ metadata
        ; metadata
        })
    protocol_part link_part metadata

let quick_link config =
  between_char '<' '>' (quick_link_aux config ~delims:quicklink_delims)

(* 1. [[url][label]] *)
(* 2. [[search]] *)
(* 3. [[ecosystem [[great]] [[Questions]]]] *)

let org_link config =
  let url_part =
    string "[[" *> take_while1 (fun c -> c <> ']') <* optional (string "][")
  in
  let label_part = take_while (fun c -> c <> ']') <* string "]]" in
  lift3
    (fun url_text label_text metadata ->
      let url =
        let url = url_text in
        if String.length url > 5 && String.sub url 0 5 = "file:" then
          File url
        else if label_text = "" then
          Search url
        else
          try
            Scanf.sscanf url "%[^:]:%[^\n]" (fun protocol link ->
                Complex { protocol; link })
          with _ -> Search url
      in
      let parser =
        many1
          (choice
             [ emphasis config
             ; latex_fragment config
             ; entity
             ; code config
             ; subscript config
             ; superscript config
             ; plain config
             ; whitespaces
             ])
      in
      let label =
        match parse_string ~consume:All parser label_text with
        | Ok result -> concat_plains result
        | Error _e -> [ Plain label_text ]
      in
      let title = None in
      let full_text = Printf.sprintf "[[%s]]%s" url_text metadata in
      Link { label; url; title; full_text; metadata })
    url_part label_part metadata

(* helper for markdown_link and markdown_image *)
let link_url_part =
  string_contains_balanced_brackets [ ('(', ')') ] eol_chars >>= fun s ->
  let len = String.length s in
  char ')' *> return s
  <|>
  if len > 0 && s.[len - 1] = ')' then
    return @@ String.sub s 0 (len - 1)
  else
    fail "link_url_part"

(* link *)
(* 1. [label](url)
   2. [label](url "title"), for example:
      My favorite search engine is [Duck Duck Go](https://duckduckgo.com "The best search engine for privacy").
*)
(* TODO: URI encode *)
let markdown_link config =
  let label_part_delims = [ '`'; '['; ']' ] in
  let label_part_choices =
    choice
      [ ( take_while1 (fun c ->
              non_eol c && (not @@ List.mem c label_part_delims))
        >>| fun s -> Plain s )
      ; ( peek_char >>= fun c ->
          match c with
          | None -> fail "finish"
          | Some '`' -> md_code <|> (any_char_string >>| fun s -> Plain s)
          | Some '[' -> page_ref <|> any_char_string >>| fun s -> Plain s
          | Some ']' ->
            available >>= fun len ->
            if len < 2 then
              fail "label_part_choices"
            else
              peek_string 2 >>= fun s ->
              if s = "](" then
                fail "finish"
              else
                any_char_string >>| fun s -> Plain s
          | Some c when is_eol c -> fail "finish"
          | Some _ -> any_char_string >>| fun s -> Plain s )
      ]
  in
  let label_part =
    string "[]("
    >>| (fun _ -> ([ Plain "" ], ""))
    <|> ( between_string "[" "]("
        @@ fix (fun m -> List.cons <$> label_part_choices <*> m <|> return [])
        >>| fun l ->
          ( concat_plains l
          , CCList.map
              (function
                | Plain s -> s
                | Code s -> "`" ^ s ^ "`"
                | _ -> "")
              l
            |> String.concat "" ) )
  in
  lift3
    (fun (label_t, label_text) url_text metadata ->
      let url, title = split_first ' ' url_text in
      let url = String.trim url in
      let title = String.trim title in
      let lowercased_url = String.lowercase_ascii url in
      let url =
        try
          Scanf.sscanf url "%[^:]:%[^\n]" (fun protocol link ->
              Complex { protocol; link })
        with _ ->
          if
            String.length url > 3
            && (ends_with lowercased_url ".md"
               || ends_with lowercased_url ".markdown")
          then
            File url
          else
            Search url
      in
      let parser =
        many1
          (choice
             [ emphasis config
             ; latex_fragment config
             ; entity
             ; code config
             ; subscript config
             ; superscript config
               (* ; plain config
                * ; whitespaces *)
             ])
      in
      let label =
        CCList.map
          (function
            | Plain s as e -> (
              match parse_string ~consume:All parser s with
              | Ok r -> r
              | Error _ -> [ e ])
            | s -> [ s ])
          label_t
        |> List.concat |> concat_plains
      in
      let title =
        if String.equal title "" || String.equal title "\"\"" then
          None
        else
          Some (String.sub title 1 (String.length title - 2))
      in
      let full_text =
        Printf.sprintf "[%s](%s)%s" label_text url_text metadata
      in
      Link { label; url; title; full_text; metadata })
    label_part link_url_part metadata

let markdown_link_or_page_ref config =
  page_ref
  >>| (fun s ->
        let inner_s = String.sub s 2 (String.length s - 4) in
        Link
          { url = Search inner_s
          ; label = [ Plain "" ]
          ; title = None
          ; full_text = s
          ; metadata = ""
          })
  <|> markdown_link config

let link config =
  match config.format with
  | Conf.Org -> org_link config
  | Conf.Markdown -> markdown_link_or_page_ref config

(* page reference *)

let nested_link _config = Nested_link.parse >>| fun s -> Nested_link s

let nested_emphasis config =
  let rec aux_nested_emphasis = function
    | Plain s -> Plain s
    | Emphasis (`Italic, [ Emphasis (`Bold, _) ]) as e -> e
    | Emphasis (typ, l) ->
      let parser =
        many1
          (choice
             [ emphasis config
             ; subscript config
             ; superscript config
             ; link config
             ; nested_link config
             ; plain config
             ])
      in
      Emphasis
        ( typ
        , CCList.map
            (function
              | Plain s as e -> (
                match parse_string ~consume:All parser s with
                | Ok [ Plain _ ] -> [ e ]
                | Ok result -> CCList.map aux_nested_emphasis result
                | Error _error -> [ e ])
              | s -> [ s ])
            l
          |> List.concat |> concat_plains )
    | Subscript _ as s -> s
    | Superscript _ as s -> s
    | Link _ as l -> l
    | Nested_link _ as nl -> nl
    | _ -> failwith "nested_emphasis"
  in
  emphasis config >>= fun e -> return (aux_nested_emphasis e)

let statistics_cookie =
  between_char '[' ']'
    (take_while1 (fun c ->
         if c = '/' || c = '%' || is_digit c then
           true
         else
           false))
  >>= fun s ->
  try
    let cookie = Scanf.sscanf s "%d/%d" (fun n n' -> Absolute (n, n')) in
    return (Cookie cookie)
  with _ -> (
    try
      let cookie = Scanf.sscanf s "%d%%" (fun n -> Percent n) in
      return (Cookie cookie)
    with _ -> fail "statistics_cookie")

(*
   Define: #+MACRO: demo =$1= ($1)
   {{{demo arg1, arg2, ..., argn}}}
*)
let macro_name = take_while1 (fun c -> c <> '}' && c <> '(' && c <> ' ')

let macro_arg =
  string "[[" *> take_while1 (fun c -> c <> ']')
  <* string "]]"
  >>| (fun s -> "[[" ^ s ^ "]]")
  <|> ( string "((" *> take_while1 (fun c -> c <> ')') <* string "))"
      >>| fun s -> "((" ^ s ^ "))" )
  <|> take_while1 (fun c -> not @@ List.mem c [ ',' ])

let macro_args =
  let args_p =
    sep_by (char ',') (optional spaces *> macro_arg <* optional spaces)
  in
  optional spaces *> args_p

let macro =
  let p =
    take_while1 (function
      | '}'
      | '\r'
      | '\n' ->
        false
      | _ -> true)
    >>= fun s ->
    match parse_string ~consume:Prefix macro_name s with
    | Ok name -> (
      let l = String.length s in
      let args = String.sub s (String.length name) (l - String.length name) in
      if String.length args == 0 then
        return (Macro { name; arguments = [] })
      else
        match parse_string macro_args ~consume:All args with
        | Ok arguments -> return (Macro { name; arguments })
        | Error e -> fail e)
    | Error _e -> fail "macro name"
  in
  between_string "{{{" "}}}" p <|> between_string "{{" "}}" p

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
              | ('+' | '.') as c ->
                (* repeat *)
                repetition_parser s date None c
              | _ ->
                (* time *)
                let time = parse_time s in
                (date, time, None))
            | Some s' ->
              let time = parse_time s in
              repetition_parser s' date time s'.[0])
        in
        match typ with
        | "Scheduled" ->
          Timestamp (Scheduled { date; wday; time; repetition; active })
        | "Deadline" ->
          Timestamp (Deadline { date; wday; time; repetition; active })
        | "Closed" ->
          Timestamp (Closed { date; wday; time; repetition; active })
        | "Clock" ->
          Timestamp (Clock (Started { date; wday; time; repetition; active }))
        | _ -> Timestamp (Date { date; wday; time; repetition; active }))
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
    string_ci rest *> ws *> any_char >>= function
    | '<' -> active_parser typ
    | '[' -> closed_parser typ
    | _ -> fail "general_timestamp"
  in
  spaces *> any_char >>= fun c ->
  let c = Char.uppercase_ascii c in
  match c with
  | '<' -> active_parser "Date"
  | '[' -> closed_parser "Date"
  | 'S' -> parse "CHEDULED:" "Scheduled"
  | 'C' -> (
    Angstrom.take 3 >>= function
    | "LOS" -> parse "ED:" "Closed"
    | "LOC" -> parse "K:" "Clock"
    | _ -> fail "general_timestamp C")
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
      | Date t
      | Scheduled t
      | Deadline t
      | Closed t ->
        t
      | _ -> failwith "illegal timestamp")
    | _ -> failwith "illegal timestamp"
  in
  lift3
    (fun clock t1 t2 ->
      let t1 = extract_time t1 in
      let t2 = extract_time t2 in
      if clock = Some "CLOCK" then
        Timestamp (Clock (Stopped { start = t1; stop = t2 }))
      else
        Timestamp (Range { start = t1; stop = t2 }))
    (spaces *> optional (letters <* char ':') <* spaces)
    (general_timestamp <* string "--")
    general_timestamp

let timestamp = range <|> general_timestamp

(* TODO: make sure it's a proper image format. *)
let markdown_image config =
  let label_part =
    string "![" *> take_while (fun c -> c <> ']') <* optional (string "](")
  in
  lift3
    (fun label_text url_text metadata ->
      let url =
        let url = url_text in
        if
          List.exists
            (ends_with (String.lowercase_ascii url))
            [ ".png"; ".jpg"; ".jpeg"; ".svg"; ".ico"; ".gif"; ".bmp" ]
        then
          File url
        else
          try
            Scanf.sscanf url "%[^:]:%[^\n]" (fun protocol link ->
                Complex { protocol; link })
          with _ -> Search url
      in
      let parser =
        many1
          (choice
             [ nested_emphasis config
             ; latex_fragment config
             ; entity
             ; code config
             ; subscript config
             ; superscript config
             ; plain config
             ; whitespaces
             ])
      in
      let label =
        match parse_string ~consume:All parser label_text with
        | Ok result -> concat_plains result
        | Error _e -> [ Plain label_text ]
      in
      let title = None in
      let full_text =
        Printf.sprintf "![%s](%s)%s" label_text url_text metadata
      in
      Link { label; url; title; full_text; metadata })
    label_part link_url_part metadata

let export_snippet =
  let name = take_while1 (fun c -> non_space_eol c && c <> ':') in
  let content =
    take_while1 (function
      | '@' -> false
      | '\r'
      | '\n' ->
        false
      | _ -> true)
  in
  between_string "@@" "@@"
    (lift2
       (fun name content -> Export_Snippet (name, content))
       (name <* string ": ")
       content)

(* src_LANG[headers]{your code} *)
let inline_source_code =
  let language = take_while1 (fun c -> is_letter c || is_digit c) in
  let options =
    between_char '[' ']' (take_while1 (fun c -> non_eol c && c <> ']'))
  in
  let code =
    between_char '{' '}' (take_while1 (fun c -> non_eol c && c <> '}'))
  in
  string "src_"
  *> lift3
       (fun language options code ->
         Inline_Source_Block { language; options; code })
       language options code

let id = ref 0

let incr_id id =
  incr id;
  !id

let footnote_inline_definition config ?(break = false) definition =
  let choices =
    [ markdown_image config
    ; nested_link config
    ; link config
    ; email
    ; link_inline
    ; radio_target
    ; target
    ; latex_fragment config
    ; nested_emphasis config
    ; entity
    ; code config
    ; subscript config
    ; superscript config
    ; plain config
    ; whitespaces
    ]
  in
  let choices =
    if break then
      List.cons hard_breakline choices
    else
      choices
  in
  let parser = many1 (choice choices) in
  match parse_string ~consume:All parser definition with
  | Ok result ->
    let result = concat_plains result in
    result
  | Error _e -> [ Plain definition ]

let latex_footnote config =
  string "[fn::" *> take_while1 (fun c -> c <> ']' && non_eol c) <* char ']'
  >>| fun definition ->
  Footnote_Reference
    { id = incr_id id
    ; name = ""
    ; definition = Some (footnote_inline_definition config definition)
    }

let markdown_footnote_reference =
  Markdown_footnote.reference >>= fun name ->
  return (Footnote_Reference { id = incr_id id; name; definition = None })

let org_inline_footnote_or_reference config =
  latex_footnote config
  <|>
  let name_part =
    string "[fn:" *> take_while1 (fun c -> c <> ':' && c <> ']' && non_eol c)
    <* optional (char ':')
  in
  let definition_part =
    take_while (fun c -> c <> ']' && non_eol c) <* char ']'
  in
  lift2
    (fun name definition ->
      let name =
        if name = "" then (
          incr id;
          "_anon_" ^ string_of_int !id
        ) else
          name
      in
      if definition = "" then
        Footnote_Reference { id = incr_id id; name; definition = None }
      else
        Footnote_Reference
          { id = incr_id id
          ; name
          ; definition = Some (footnote_inline_definition config definition)
          })
    name_part definition_part

let inline_footnote_or_reference config =
  match config.format with
  | Conf.Org -> org_inline_footnote_or_reference config
  | Conf.Markdown -> markdown_footnote_reference

let break_or_line =
  let line = line >>= fun s -> return (Plain s) in
  choice [ line; hard_breakline; breakline ]

(* choice [line; breakline; allow_breakline] *)

(* TODO: Allow to disable this *)
let block_reference _config =
  Block_reference.parse >>= fun s -> return @@ Block_reference s

let hash_tag = Hash_tag.parse >>| fun s -> Tag s

let inline_hiccup = Hiccup.parse >>| fun s -> Inline_Hiccup s

let inline_html = Raw_html.parse >>| fun s -> Inline_Html s

(* TODO: configurable, re-order *)
let inline_choices config =
  let is_markdown = config.format = Conf.Markdown in
  let p =
    if is_markdown then
      peek_char_fail >>= function
      | '\n' -> breakline
      | '#' -> hash_tag
      | '*'
      | '~' ->
        nested_emphasis config
      | '_' -> nested_emphasis config <|> subscript config
      | '^' -> nested_emphasis config <|> superscript config
      | '$' -> latex_fragment config
      | '\\' -> latex_fragment config <|> entity
      | '[' ->
        nested_link config <|> link config <|> timestamp
        <|> inline_footnote_or_reference config
        <|> statistics_cookie <|> inline_hiccup
      | '<' -> quick_link config <|> timestamp <|> inline_html <|> email
      | '{' -> macro
      | '!' -> markdown_image config
      | '@' -> export_snippet
      | '`' -> code config
      | 'S'
      | 'C'
      | 'D'
      | 's'
      | 'c'
      | 'd' ->
        timestamp
      | '(' -> block_reference config
      | ' ' -> Markdown_line_breaks.parse >>| fun _ -> Hard_Break_Line
      | _ -> link_inline
    else
      peek_char_fail >>= function
      | '\n' -> breakline
      | '#' -> hash_tag
      | '*'
      | '/'
      | '+' ->
        nested_emphasis config
      | '_' -> nested_emphasis config <|> subscript config
      | '^' -> nested_emphasis config <|> superscript config
      | '$' -> latex_fragment config
      | '\\' ->
        org_hard_breakline
        >>| (fun _ -> Hard_Break_Line)
        <|> latex_fragment config <|> entity
      | '[' ->
        nested_link config <|> link config <|> timestamp
        <|> inline_footnote_or_reference config
        <|> statistics_cookie <|> inline_hiccup
      | '<' -> target <|> radio_target <|> timestamp <|> inline_html <|> email
      | '{' -> macro
      | '!' -> markdown_image config
      | '@' -> export_snippet
      | '=' -> code config <|> verbatim
      | '~' -> code config
      | 'S'
      | 'C'
      | 'D'
      | 's'
      | 'c'
      | 'd' ->
        timestamp
      | '(' -> block_reference config
      | _ -> link_inline
  in
  p <|> plain config

(* let choices =
 *   [
 *     latex_fragment config            (\* '$' '\' *\)
 *   ; inline_footnote_or_reference config        (\* 'f', fn *\)
 *   ; block_reference config               (\* (()) *\)
 *   ; hard_breakline            (\* "\\" *\)
 *   ; breakline                 (\* '\n' *\)
 *   ; timestamp                 (\* '<' '[' 'S' 'C' 'D'*\)
 *   (\* ; entity                    (\\* '\' *\\) *\)
 *   ; macro                     (\* '{' *\)
 *   ; statistics_cookie         (\* '[' *\)
 *   (\* ; markdown_image config *\)
 *   ; link config                      (\* '[' [[]] *\)
 *   ; link_inline config               (\*  *\)
 *   ; email
 *   ; export_snippet
 *   ; radio_target              (\* "<<<" *\)
 *   ; target                    (\* "<" *\)
 *   ; verbatim                  (\*  *\)
 *   ; code config                      (\* '=' *\)
 *   ; nested_emphasis config
 *   ; subscript config                 (\* '_' "_{" *\)
 *   ; superscript config               (\* '^' "^{" *\)
 *   ; plain config
 *   ] in
 * choice choices *)

let parse config =
  many1 (inline_choices config) >>| (fun l -> concat_plains l) <?> "inline"

let string_of_url = function
  | File s
  | Search s ->
    s
  | Complex { link; protocol = "file" } -> link
  | Complex { link; protocol } -> protocol ^ ":" ^ link

let rec ascii = function
  | Footnote_Reference ref -> Option.map_default asciis "" ref.definition
  | Link l -> asciis l.label
  | Emphasis (_, t) -> asciis t
  | Subscript l
  | Superscript l ->
    asciis l
  | Latex_Fragment (Inline s)
  | Plain s
  | Verbatim s ->
    s
  | Entity e -> e.Entity.unicode
  | _ -> ""

and asciis l = String.concat "" (CCList.map ascii l)

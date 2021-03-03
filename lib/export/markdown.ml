open Prelude
open Type
open Inline
open Document

type state = { outside_em_symbol : char option }

let rec inline state config (t : Inline.t) : string list =
  match t with
  | Emphasis em -> emphasis state config em
  | Break_Line -> [ "\n" ]
  | Hard_Break_Line -> [ "  \n" ]
  | Verbatim s -> [ s ]
  | Code s -> [ "`"; s; "`" ] (* it's inline code *)
  | Tag s -> [ "#"; s ]
  | Spaces s -> [ s ]
  | Plain s -> [ s ]
  | Link l -> inline_link l
  | Nested_link l -> inline_nested_link l
  | Target s -> [ "<<"; s; ">>" ]
  | Subscript tl -> inline_subscript state config tl
  | Superscript tl -> inline_superscript state config tl
  | Footnote_Reference fr -> footnote_reference fr
  | Cookie c -> cookie c
  | Latex_Fragment lf -> latex_fragment lf
  | Macro m -> macro m
  | Entity e -> entity e
  | Timestamp t -> timestamp t
  | Radio_Target s -> [ "<<<"; s; ">>>" ] (* FIXME: not parsed in md parser? *)
  | Export_Snippet (name, content) -> [ "@@"; name; ": "; content; "@@" ]
  | Inline_Source_Block { language; options; code } ->
    [ "src_"; language; "["; options; "]{"; code; "}" ]
  | Email e -> [ "<"; Email_address.to_string e; ">" ]
  | Block_reference s -> [ "(("; s; "))" ]
  | Inline_Hiccup s -> [ s ]

and emphasis state config (typ, tl) =
  let outside_em_symbol = state.outside_em_symbol in
  let wrap_with tl s =
    let outside_em_symbol =
      match s.[0] with
      | ('*' | '_') as e -> Some e
      | _ -> None
    in
    List.flatten
      [ [ s ]
      ; List.flatten @@ CCList.map (inline { outside_em_symbol } config) tl
      ; [ s ]
      ]
  in
  match typ with
  | `Bold ->
    wrap_with tl
      (if outside_em_symbol = Some '*' then
        "__"
      else
        "**")
  | `Strike_through -> wrap_with tl "~~"
  | `Highlight -> wrap_with tl "^^"
  | `Italic ->
    wrap_with tl
      (if outside_em_symbol = Some '*' then
        "_"
      else
        "*")
  | `Underline ->
    List.flatten @@ CCList.map (inline { outside_em_symbol } config) tl

and inline_link { full_text; _ } = [ full_text ]

and inline_nested_link { content; _ } = [ content ]

and inline_subscript state config tl =
  List.flatten
    [ [ "_{" ]; List.flatten @@ CCList.map (inline state config) tl; [ "}" ] ]

and inline_superscript state config tl =
  List.flatten
    [ [ "^{" ]; List.flatten @@ CCList.map (inline state config) tl; [ "}" ] ]

and footnote_reference { name; _ } = [ "["; name; "]" ]

and cookie = function
  | Percent v -> [ "["; string_of_int v; "%]" ]
  | Absolute (current, total) ->
    [ "["; string_of_int current; "/"; string_of_int total; "]" ]

and latex_fragment = function
  | Inline s -> [ "$"; s; "$" ]
  | Displayed s -> [ "$$"; s; "$$" ]

and macro { name; arguments } =
  if List.length arguments > 0 then
    [ "{{{"; name; "("; String.concat "," arguments; ")}}}" ]
  else
    [ "{{{"; name; "}}}" ]

and entity { unicode; _ } = [ unicode ]

and timestamp t =
  match t with
  | Scheduled tt -> [ "SCHEDULED: "; Timestamp.to_string tt ]
  | Deadline tt -> [ "DEADLINE: "; Timestamp.to_string tt ]
  | Date tt -> [ Timestamp.to_string tt ]
  | Closed tt -> [ "CLOSED: "; Timestamp.to_string tt ]
  | Clock (Started tt) -> [ "CLOCK: "; Timestamp.to_string tt ]
  | Clock (Stopped rt) -> [ "CLOCK: "; Range.to_string rt ]
  | Range rt -> [ Range.to_string rt ]

let rec block config t =
  match t with
  | Paragraph l -> CCList.map (inline config) l
  | _ -> failwith "block"

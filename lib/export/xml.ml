(* taken from mlorg *)

type t =
  | Empty
  | Data of string
  | Block of string * (string * string) list * t list
  | Raw of string
  | List of t list

let empty = Empty

let block ?(attr = []) name children = Block (name, attr, children)

let data s = Data s

let raw s = Raw s

let list = function [] -> Empty | l -> List l

let quote, _ = ('"', '"')

let output_string_rewrite fd s =
  let l = String.length s in
  for p = 0 to l - 1 do
    match s.[p] with
    | '>' -> output_string fd "&gt;"
    | '<' -> output_string fd "&lt;"
    | '&' ->
      if p < l - 1 && s.[p + 1] = '#' then output_char fd '&'
      else output_string fd "&amp;"
    | '\'' -> output_string fd "&apos;"
    | c when c = quote -> output_string fd "&quot;"
    | c -> output_char fd c
  done

let output_attribs fd =
  let write (name, value) =
    Printf.fprintf fd "%s=\"" name ;
    output_string_rewrite fd value ;
    Printf.fprintf fd "\""
  in
  let rec aux = function
    | [x] -> write x
    | [] -> ()
    | t :: q -> write t ; Printf.fprintf fd " " ; aux q
  in
  aux

let indent fd num = output_string fd (String.make num ' ')

let write_string o s =
  output_string o s;
  output_char o '\000'

let output_lines ?(rewrite = true) fd indent_level lines =
  match Prelude.lines lines with
  | t :: q ->
    let output =
      if rewrite then output_string_rewrite fd else write_string fd
    in
    output t ;
    List.iter
      (fun s -> output_char fd '\n' ; indent fd indent_level ; output s)
      q ;
    if String.length lines >= 1 then
      if lines.[String.length lines - 1] = '\n' then output "\n"
  | [] -> ()

let output ?(offset = 0) fd inlines prep_inlines exceptions space_significants
    trees =
  let rec write ?(ctx_inline = false) indent_level = function
    | Empty -> ()
    | Data s -> output_lines fd indent_level s
    | Raw s -> Printf.fprintf fd "%s" s
    | List l -> List.iter (write indent_level) l
    | Block (name, attribs, children) ->
      let inline = List.mem name inlines in
      let close_tag = children = [] && not (List.mem name exceptions) in
      let is_child_inline =
        List.exists
          (function
            | Block (name, _, _) -> List.mem name inlines | _ -> true)
          children
      in
      let lvl =
        if not (List.mem name space_significants) then indent_level + 2
        else 0
      in
      if not inline then (
        if ctx_inline then output_string fd "\n" ;
        indent fd indent_level ) ;
      Printf.fprintf fd "<%s" name ;
      if attribs <> [] then ( output_char fd ' ' ; output_attribs fd attribs ) ;
      if close_tag then output_string fd " />"
      else (
        output_string fd ">" ;
        if (not inline) && not is_child_inline then output_string fd "\n" ;
        List.iter (write ~ctx_inline:is_child_inline lvl) children ;
        if
          lvl > 0 && (not inline)
          && ( (not (List.mem name prep_inlines))
               || List.exists
                 (function
                   | Block (k, _, _child) -> List.mem k prep_inlines
                   | _ -> false)
                 children )
        then (
          ( if children <> [] then
              match Prelude.last_opt children with
              | Some (Data _) -> output_string fd "\n"
              | Some _ | None -> ()) ;
          indent fd indent_level ) ;
        Printf.fprintf fd "</%s>" name ) ;
      if not inline then output_char fd '\n'
  in
  List.iter (write offset) trees ;
  output_string fd "\n"

let output_xhtml ?offset chan =
  output chan ?offset
    ["u"; "i"; "em"; "b"; "img"; "a"; "code"; "sup"; "sub"; "abbr"; "span"]
    [ "p"
    ; "li"
    ; "ol"
    ; "dt"
    ; "td"
    ; "h1"
    ; "h2"
    ; "h3"
    ; "h4"
    ; "h5"
    ; "hr"
    ; "th"
    ; "ul"
    ; "title" ]
    ["div"; "span"; "ul"] ["pre"; "code"]

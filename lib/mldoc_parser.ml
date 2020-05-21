open Angstrom

let list_content_parsers config =
  many1 (choice [
      Table.parse config
    ; Block.parse config
    ; Latex_env.parse config
    ; Hr.parse config
    ; Block.results
    ; Comment.parse config
    ; Paragraph.parse config [ Paragraph.sep
                             ; Table.parse config
                             ; Block.parse config
                             ; Block.results
                             ; Latex_env.parse config
                             ; Hr.parse config
                             ; Comment.parse config]
    ])

(* Orders care *)
let interrupt_parsers config =
  [ Directive.parse
  ; Heading.parse config
  ; Paragraph.sep
  ; Table.parse config
  ; Lists.parse config (list_content_parsers config)
  ; Drawer.parse
  ; Block.parse config
  ; Latex_env.parse config
  ; Hr.parse config
  ; Block.results
  ; Comment.parse config
  ]

let parsers config =
  let parsers = List.append (interrupt_parsers config) [Paragraph.parse config (interrupt_parsers config)] in
  let choices = choice parsers
  in
  let parse = many choices in
  Markdown_front_matter.parse >>=
  fun fm_result ->
  parse >>= fun result ->
  return (List.append fm_result result)

let parse config input =
  match parse_string (parsers config) input with
  | Ok result -> List.concat result
  | Error err -> failwith err

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

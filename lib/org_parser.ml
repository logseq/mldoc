open Angstrom

let list_content_parsers config =
  many1 (choice [
      Table.parse
    ; Block.parse config
    ; Latex_env.parse
    ; Hr.parse
    ; Block.results
    ; Comment.parse
    ; Paragraph.parse config [ Table.parse
                             ; Block.parse config
                             ; Block.results
                             ; Latex_env.parse
                             ; Hr.parse
                             ; Comment.parse]
    ])

(* Orders care *)
let interrupt_parsers config =
  [ Heading.parse
  ; Table.parse
  ; Lists.parse (list_content_parsers config)
  ; Block.parse config
  ; Directive.parse
  ; Drawer.parse
  ; Latex_env.parse
  ; Hr.parse
  ; Block.results
  ; Comment.parse
  ]

let parsers config =
  let parsers = List.append (interrupt_parsers config) [Paragraph.parse config (interrupt_parsers config)] in
  let choices = choice parsers
  in
  many1 choices

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

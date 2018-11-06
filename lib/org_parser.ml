open Angstrom
open Org

let list_content_parsers =
  many1 (choice [
      Table.parse
    ; Block.parse
    ; Directive.parse
    ; Drawer.parse
    ; Latex_env.parse
    ; Hr.parse
    ; Comment.parse
    ; Paragraph.parse [ Table.parse
                      ; Block.parse
                      ; Directive.parse
                      ; Drawer.parse
                      ; Latex_env.parse
                      ; Hr.parse
                      ; Comment.parse]
    ])

(* Orders care *)
let interrupt_parsers =
  [ Heading.parse
  ; Table.parse
  ; Lists.parse list_content_parsers
  ; Block.parse
  ; Directive.parse
  ; Drawer.parse
  ; Latex_env.parse
  ; Hr.parse
  ; Comment.parse
  ]

let parsers =
  let parsers = List.append interrupt_parsers [Paragraph.parse interrupt_parsers] in
  let choices = choice parsers
  in
  many1 choices

let parse input =
  match parse_string parsers input with
  | Ok result -> List.concat result
  | Error err -> failwith err

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

(*
let text = load_file "/tmp/syntax.org";;

time @@ fun _ -> parse text;;
*)

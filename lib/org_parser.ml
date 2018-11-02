open Angstrom
open Org
(* TODO: interrupt
   1. List
   2. Paragraph
*)

(* Order cares *)
let rec interrupt_parsers =
  [
    Heading.parse             (* 100 *)
  ; Table.parse               (* 12 *)
  ; Lists.parse               (* 10 *)
  (* ; Block.parse               (\* 10 *\) *)
  ; Directive.parse           (* 10 *)
  ; Drawer.parse              (* 10 *)
  (* ; Latex_env.parse           (\* 10 *\) *)
  (* ; Math.parse                (\* 2 *\) *)
  ; Hr.parse                  (* 1 *)
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

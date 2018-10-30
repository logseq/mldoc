open Angstrom

let parsers =
  choice
    [
      Heading.parse             (* 100 *)
    (* ; Table.parse               (\* 12 *\) *)
    (* ; Lists.parse               (\* 10 *\) *)
    (* ; Block.parse               (\* 10 *\) *)
    (* ; Directive.parse           (\* 10 *\) *)
    (* ; Drawer.parse              (\* 10 *\) *)
    (* ; Latex_env.parse           (\* 10 *\) *)
    (* ; Math.parse                (\* 2 *\) *)
    (* ; Hr.parse                  (\* 1 *\) *)
    (* ; Paragraph.parse           (\* 0 *\) *)
    ]

let parse input =
  match parse_string parsers input with
  | Ok result -> result
  | Error err -> failwith err

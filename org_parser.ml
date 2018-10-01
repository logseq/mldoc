open Angstrom

let parse input =
  match parse_string Heading.parse input with
  | Ok result -> result
  | Error err -> failwith err

let _ =
  let open Js_of_ocaml in
  Js.export_all
    (object%js
       method parse input =
         let str = Js.to_string input in
         parse @@ str
    end)

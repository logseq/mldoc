open Angstrom

let parse input =
  match parse_string Heading.level input with
  | Ok l -> List.length l
  | Error err -> failwith err

let _ =
  let open Js_of_ocaml in
  Js.export "OrgParser"
    (object%js
       method parse input =
         let str = Js.to_string input in
         parse @@ str

       method add x y = x + y
    end)

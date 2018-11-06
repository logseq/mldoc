open Org_parser

let _ =
  let open Js_of_ocaml in
  Js.export_all
    (object%js
      method parse input =
        let str = Js.to_string input in
        parse str |> ast_to_json |> Js.string
    end)

let _ =
  let open Js_of_ocaml in
  Js.export_all
    (object%js
      method parse input =
        let str = Js.to_string input in
        Org_parser.parse str
    end)

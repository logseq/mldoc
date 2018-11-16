open Mldoc_org
open Mldoc_org.Parser

let ast_to_json ast =
  Type.blocks_to_yojson ast |> Yojson.Safe.to_string

let generate backend doc output =
  let export = Exporters.find backend in
  Exporters.run export doc output

let _ =
  let open Js_of_ocaml in
  Js.export "MldocOrg"
    (object%js
      method parseJson input =
        let str = Js.to_string input in
        parse str |> ast_to_json |> Js.string

      method parseHtml input =
        let str = Js.to_string input in
        let ast = parse str in
        let document = Document.from_ast None ast in
        let buffer = Buffer.create 1024 in
        let _ = Sys_js.set_channel_flusher stdout (fun s ->
            Buffer.add_string buffer s
          ) in
        generate "html" document stdout;
        Js_of_ocaml.Js.string (Buffer.contents buffer)
    end)

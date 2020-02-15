open Mldoc_org
open Mldoc_org.Parser

let ast_to_json ast =
  Type.blocks_to_yojson ast |> Yojson.Safe.to_string

let json_to_ast json =
  Type.blocks_of_yojson json

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

      method jsonToAst input =
        let json_str = Js.to_string input in
        let json = Yojson.Safe.from_string json_str in
        json_to_ast json

      method jsonToHtmlStr input =
        let json_str = Js.to_string input in
        let json = Yojson.Safe.from_string json_str in
        match json_to_ast json with
        | Ok blocks ->
          let buffer = Buffer.create 1024 in
          let _ = Sys_js.set_channel_flusher stdout (fun s ->
              Buffer.add_string buffer s
            ) in
          let xml_blocks = Html.blocks blocks in
          let _ = Xml.output_xhtml stdout xml_blocks in
          Js_of_ocaml.Js.string (Buffer.contents buffer)
        | Error e ->
          Js_of_ocaml.Js.string "parse error"

      method inlineListToHtmlStr input =
        let json_str = Js.to_string input in
        let json = Yojson.Safe.from_string json_str in
        match Type.inline_list_of_yojson json with
        | Ok inline_list ->
          let buffer = Buffer.create 1024 in
          let _ = Sys_js.set_channel_flusher stdout (fun s ->
              Buffer.add_string buffer s
            ) in
          let _ = Xml.output_xhtml stdout (Html.map_inline inline_list) in
          Js_of_ocaml.Js.string (Buffer.contents buffer)
        | Error e ->
          Js_of_ocaml.Js.string "parse error"
    end)

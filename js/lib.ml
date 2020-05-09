open Mldoc_org
open Mldoc_org.Parser
open Mldoc_org.Config

let ast_to_json ast =
  Type.blocks_to_yojson ast |> Yojson.Safe.to_string

let generate backend config doc output =
  let export = Exporters.find backend in
  Exporters.run export config doc output

let _ =
  let open Js_of_ocaml in
  Js.export "MldocOrg"
    (object%js
      method parseJson input config_json =
        let config_json = Js.to_string config_json in
        let config_json = Yojson.Safe.from_string config_json in
        match Config.of_yojson config_json with
        | Ok config -> begin
            let str = Js.to_string input in
            parse config str |> ast_to_json |> Js.string
          end
        | Error e ->
          Js_of_ocaml.Js.string ("Config error: " ^ e)

      method anchorLink s =
        let s = Js.to_string s in
        Js_of_ocaml.Js.string (Heading.anchor_link s)
    end)

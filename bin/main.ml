open Mldoc_org
open Mldoc_org.Parser

let generate backend doc output =
  let export = Exporters.find backend in
  Exporters.run export doc output

let _ =
  let _ = Printexc.record_backtrace true in
  let text = load_file "/tmp/test.org" in
  let ast = parse text in
  let document = Document.from_ast None ast in
  generate "html" document stdout

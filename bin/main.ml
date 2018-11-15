open Mldoc_org
open Mldoc_org.Parser
open Lwt

let generate backend doc output =
  let export = Exporters.find backend in
  Exporters.run export doc output

let rec read_lines () =
  try let line = read_line () in
    line :: read_lines ()
  with
    End_of_file -> []

let _ =
  let _ = Printexc.record_backtrace true in

  let lines = read_lines () in
  let ast = parse (String.concat "\n" lines) in
  let document = Document.from_ast None ast in
  return (generate "html" document stdout)

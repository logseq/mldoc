open Mldoc_org
open Mldoc_org.Parser
open Mldoc_org.Conf
open Lwt
open Cmdliner

(* stdin *)
let read_lines () =
  Lwt_io.read_lines Lwt_io.stdin |> Lwt_stream.to_list

(* file *)
let from_file filename =
  Lwt_io.lines_of_file filename |> Lwt_stream.to_list

let generate backend output opts filename =
  let lines = if filename = "-" then
      read_lines ()
    else from_file filename
  in
  lines >>= function lines ->
    let config = {toc = true; heading_number = true; keep_line_break = false; } in
    let ast = parse config (String.concat "\n" lines) in
    let document = Document.from_ast None ast in
    let export = Exporters.find backend in
    let module E = (val export : Exporter.Exporter) in
    let output = if output = "" then E.default_filename filename else output in
    let fdout = if output = "-" then stdout else open_out output in
    (* FIXME: parse *)
    let result = Exporters.run export config document fdout in
    return result

(* Cmd liner part *)

(* Commonon options *)
let output =
  let doc = "Write the generated file to $(docv). " in
  Arg.(value & opt string "" & info ["o"; "output"] ~docv:"OUTPUT-FILE" ~doc)

let backend =
  let doc = "Uses $(docv) to generate the output. (`-` for stdout)" in
  Arg.(value & opt string "html" & info ["b"; "backend"] ~docv:"BACKEND" ~doc)

let filename =
  let doc = "The input filename to use. (`-` for stdin) " in
  Arg.(value & pos 0 string "-" & info [] ~docv:"FILENAME" ~doc)

let options =
  let doc =
    "Extra option to use to configure the behaviour. (Can be used multiple \
     times)"
  in
  Arg.(
    value
    & opt_all (pair ~sep:'=' string string) []
    & info ["x"; "option"] ~docv:"OPTIONS" ~doc)

let cmd = Term.(pure generate $ backend $ output $ options $ filename)

let doc = "converts org-mode files into various formats"

let options =
  []

let man =
  [ `S "DESCRIPTION"
  ; `P
      "$(tname) can currently converts org-mode files into other formats such as
       HTML." ]
  @ options

let infos = Term.info "mldoc_org" ~version:"0" ~doc ~man

let main () =
  match Term.eval (cmd, infos) with
  | `Error _ -> exit 1
  | `Ok expr -> Lwt_main.run expr
  | _ -> exit 0

let () =
  let _ = Printexc.record_backtrace true in
  if not !Sys.interactive then
    main ()

open Mldoc.Parser
open Mldoc.Conf

open Core
open Core_bench

let text_org = load_file "./examples/syntax.org"
let text_md = load_file "./examples/syntax.md"

let main () =
  Command.run (Bench.make_command [
      Bench.Test.create ~name:"Mldoc Org mode parser" (fun () ->
          let config = {toc = true; heading_number = true; keep_line_break = false; format = "Org"; } in
          ignore (parse config text_org));
      Bench.Test.create ~name:"Mldoc Markdown parser" (fun () ->
          let config = {toc = true; heading_number = true; keep_line_break = false; format = "Markdown"; } in
          ignore (parse config text_md));
    ])

let () = main ()

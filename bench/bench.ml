open Mldoc.Parser
open Mldoc.Conf
open Core
open Core_bench

let doc_org = load_file "./examples/doc.org"

let syntax_org = load_file "./examples/syntax.org"

let syntax_md = load_file "./examples/syntax.md"

let main () =
  Command.run
    (Bench.make_command
       [ Bench.Test.create ~name:"Mldoc Org mode parser (doc)" (fun () ->
             let config =
               { toc = true
               ; heading_number = true
               ; keep_line_break = false
               ; format = Org
               ; heading_to_list = false
               ; exporting_keep_properties = false
               }
             in
             ignore (parse config doc_org))
       ; Bench.Test.create ~name:"Mldoc Org mode parser (syntax)" (fun () ->
             let config =
               { toc = true
               ; heading_number = true
               ; keep_line_break = false
               ; format = Org
               ; heading_to_list = false
               ; exporting_keep_properties = false
               }
             in
             ignore (parse config syntax_org))
       ; Bench.Test.create ~name:"Mldoc Markdown parser" (fun () ->
             let config =
               { toc = true
               ; heading_number = true
               ; keep_line_break = false
               ; format = Markdown
               ; heading_to_list = false
               ; exporting_keep_properties = false
               }
             in
             ignore (parse config syntax_md))
       ])

let () = main ()

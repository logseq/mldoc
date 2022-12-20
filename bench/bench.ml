open Mldoc.Parser
open Mldoc.Conf
open Core
open Core_bench
open Angstrom

let doc_org = load_file "./examples/doc.org"

let syntax_md = load_file "./examples/syntax.md"

let config =
  { toc = true
  ; parse_outline_only = false
  ; heading_number = true
  ; keep_line_break = false
  ; format = Org
  ; heading_to_list = false
  ; exporting_keep_properties = false
  ; inline_type_with_pos = false
  ; inline_skip_macro = false
  ; export_md_indent_style = Dashes
  ; export_md_remove_options = []
  ; hiccup_in_block = true
  }

let outline_config = { config with parse_outline_only = true }

let main () =
  Command_unix.run
    (Bench.make_command
       [ Bench.Test.create ~name:"Inline parse (doc)" (fun () ->
             let p = Inline.parse config in
             ignore (parse_string ~consume:All p syntax_md))
       ; Bench.Test.create ~name:"Inline parse outline (doc)" (fun () ->
             let p = Outline_inline.parse outline_config in
             ignore (parse_string ~consume:All p syntax_md))
       ; Bench.Test.create ~name:"Mldoc Org mode parser" (fun () ->
             ignore (parse config doc_org))
       ; Bench.Test.create ~name:"Mldoc Org mode parser (outline only)"
           (fun () -> ignore (parse outline_config doc_org))
       ; Bench.Test.create ~name:"Mldoc Markdown parser" (fun () ->
             ignore (parse config syntax_md))
       ; Bench.Test.create ~name:"Mldoc Markdown parser (outline only)"
           (fun () -> ignore (parse outline_config syntax_md))
       ])

let () = main ()

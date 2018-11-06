open Org_parser

open Core
open Core_bench.Std

let text_org = load_file "/tmp/syntax.org"

let main () =
  Command.run (Bench.make_command [
    Bench.Test.create ~name:"Org parser" (fun () ->
      ignore (parse text_org));
  ])

let () = main ()

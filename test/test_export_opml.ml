let default_config : Conf.t =
  { toc = true
  ; heading_number = true
  ; keep_line_break = false
  ; format = Conf.Markdown
  ; heading_to_list = false
  ; exporting_keep_properties = false
  ; inline_type_with_pos = false
  ; export_md_indent_style = Conf.Dashes
  ; export_md_remove_options = []
  ; hiccup_in_block = true
  }

let check_aux ?(config = default_config) source expect =
  let tl = Mldoc_parser.parse config source in
  let buf = Buffer.create 100 in
  let output_buf = Xmlm.make_output ~indent:(Some 2) (`Buffer buf) in
  let _ = Opml.blocks Reference.empty_parsed_t tl "title" output_buf in
  fun _ ->
    Alcotest.check Alcotest.string "check exported string" expect
      (Buffer.contents buf)

let testcases =
  List.map (fun (case, level, f) -> Alcotest.test_case case level f)

let testcase_list =
  [ ( "export opml"
    , testcases
        [ ( "normal"
          , `Quick
          , check_aux "- line1\n  - line2\n    line3\n    - line4"
              "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
               <opml version=\"2.0\">\n\
              \  <head>\n\
              \    <title>\n\
              \      title\n\
              \    </title>\n\
              \  </head>\n\
              \  <body>\n\
              \    <outline text=\"line1\">\n\
              \      <outline text=\"line2\" _note=\"    line3&#10;\">\n\
              \        <outline text=\"line4\"/>\n\
              \      </outline>\n\
              \    </outline>\n\
              \  </body>\n\
               </opml>" )
        ; ( "normal (1)"
          , `Quick
          , check_aux
              "- ## line1\n\
              \  - ## TODO line2\n\
              \    line3\n\
              \    - ##  LATER [#A] line4"
              "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
               <opml version=\"2.0\">\n\
              \  <head>\n\
              \    <title>\n\
              \      title\n\
              \    </title>\n\
              \  </head>\n\
              \  <body>\n\
              \    <outline text=\"## line1\">\n\
              \      <outline text=\"## TODO line2\" _note=\"    line3&#10;\">\n\
              \        <outline text=\"## LATER [#A] line4\"/>\n\
              \      </outline>\n\
              \    </outline>\n\
              \  </body>\n\
               </opml>" )
        ] )
  ]

let () = Alcotest.run "export-opml" testcase_list

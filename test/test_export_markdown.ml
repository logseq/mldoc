let default_config : Conf.t =
  { toc = true
  ; heading_number = true
  ; keep_line_break = false
  ; format = Conf.Markdown
  ; heading_to_list = false
  ; exporting_keep_properties = false
  ; ignore_heading_list_marker = false
  ; inline_type_with_pos = false
  }

let refs : Reference.parsed_t =
  { parsed_embed_blocks =
      [ ( "ref1"
        , ( [ Type.Heading
                { Type.title =
                    Type_op.inline_list_with_none_pos
                      [ Inline.Plain "ref1-text" ]
                ; tags = []
                ; marker = None
                ; level = 1
                ; numbering = None
                ; priority = None
                ; anchor = "ref1-text"
                ; meta = { Type.timestamps = []; properties = [] }
                ; unordered = true
                ; size = None
                }
            ; Type.Property_Drawer
                [ ("id", "60d2ead8-23c1-4617-b2df-4ef0dcfbbf2e") ]
            ]
          , [ Type.Heading
                { Type.title =
                    Type_op.inline_list_with_none_pos
                      [ Inline.Plain "ref1-text" ]
                ; tags = []
                ; marker = None
                ; level = 1
                ; numbering = None
                ; priority = None
                ; anchor = "ref1-text"
                ; meta = { Type.timestamps = []; properties = [] }
                ; unordered = true
                ; size = None
                }
            ; Type.Property_Drawer
                [ ("id", "60d2ead8-23c1-4617-b2df-4ef0dcfbbf2e") ]
            ] ) )
      ]
  ; parsed_embed_pages = []
  }

let check_aux source expect =
  let tl = Mldoc_parser.parse default_config source in
  let ol = Markdown.blocks refs default_config tl in
  fun _ ->
    Alcotest.check Alcotest.string "check exported string" expect
      (Output.to_string ol)

let testcases =
  List.map (fun (case, level, f) -> Alcotest.test_case case level f)

let export_md =
  [ ( "replace block-ref"
    , testcases
        [ ( "merge paragraph"
          , `Quick
          , check_aux "- text1 ((ref1)) text2" "- text1 ref1-text text2" )
        ] )
  ]

let () = Alcotest.run "exporting-md" export_md

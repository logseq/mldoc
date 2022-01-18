let default_config : Conf.t =
  { toc = true
  ; parse_outline_only = false
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

let check_aux ?(config = default_config) source expect =
  let tl = Mldoc_parser.parse config source in
  let ol = Markdown.blocks refs config tl in
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
  ; ( "export md"
    , testcases
        [ ( "(1)"
          , `Quick
          , check_aux "- line1\n  line2\n  - line3\n  line4"
              "- line1\n  line2\n\t- line3\n\t  line4" )
        ; ( "(2)"
          , `Quick
          , check_aux "- line1\n  line2\n  - > line3\n    > line4"
              "- line1\n  line2\n\t-\n\t  > line3\n\t  line4\n" )
        ; ( "(3)"
          , `Quick
          , check_aux
              "- line1\n\
              \  line2\n\
              \  - > line3\n\
              \    > line4\n\
              \    - line5\n\
              \      [[line6]]"
              "- line1\n\
              \  line2\n\
               \t-\n\
               \t  > line3\n\
               \t  line4\n\n\
               \t\t- line5\n\
               \t\t  [[line6]]" )
        ; ( "(4)"
          , `Quick
          , check_aux
              "- line1\n\
              \  line2\n\
              \  - > line3\n\
              \    > line4\n\
              \    - line5\n\
              \      [[line6]]\n\
              \      -\n\
               \tprop:: hahaha\n\
               \t```\n\
               \t  dwdw\n\
               \t  jdiejdie\n\
               \t```"
              "- line1\n\
              \  line2\n\
               \t-\n\
               \t  > line3\n\
               \t  line4\n\n\
               \t\t- line5\n\
               \t\t  [[line6]]\n\
               \t\t\t-\n\
               \t\t\t  ```\n\
               \t\t\t  dwdw\n\
               \t\t\t  jdiejdie\n\
               \t\t\t  ```" )
        ; ( "(5)"
          , `Quick
          , check_aux "- `key`: content **bold**test"
              "- `key`: content **bold**test" )
        ; ("(6)", `Quick, check_aux "## heading" "## heading")
        ; ( "(7)"
          , `Quick
          , check_aux "- **bold** *italic*\ntest" "- **bold** *italic*\n  test"
          )
        ; ( "indent style='spaces' (1)"
          , `Quick
          , check_aux
              ~config:
                { default_config with export_md_indent_style = Conf.Spaces }
              "- line1\n  line2\n  - > line3\n    > line4"
              "line1\nline2\n\t> line3\n\tline4\n" )
        ; ( "indent style='spaces' (2)"
          , `Quick
          , check_aux
              ~config:
                { default_config with export_md_indent_style = Conf.Spaces }
              "- line1\n\
              \  line2\n\
              \  - > line3\n\
              \    > line4\n\
              \    - line5\n\
              \      [[line6]]"
              "line1\nline2\n\t> line3\n\tline4\n\n\t\tline5\n\t\t[[line6]]" )
        ; ( "indent style='no-indent' (2)"
          , `Quick
          , check_aux
              ~config:
                { default_config with export_md_indent_style = Conf.NoIndent }
              "- line1\n\
              \  line2\n\
              \  - > line3\n\
              \    > line4\n\
              \    - line5\n\
              \      [[line6]]"
              "line1\nline2\n> line3\nline4\n\nline5\n[[line6]]" )
        ; ( "heading size"
          , `Quick
          , check_aux "- # line1\n  - ##  TODO line2\n  - line3"
              "- # line1\n\t- ## TODO line2\n\t- line3" )
        ; ( "replace cloze with its content (1)"
          , `Quick
          , check_aux "- {{cloze content1,content2}}" "- content1,content2" )
        ; ( "replace cloze with its content (2)"
          , `Quick
          , check_aux "- {{cloze (content1,content2)}}" "- (content1,content2)"
          )
        ] )
  ]

let () = Alcotest.run "export-md" export_md

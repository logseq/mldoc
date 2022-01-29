let default_config : Conf.t =
  { toc = true
  ; parse_outline_only = false
  ; heading_number = true
  ; keep_line_break = false
  ; format = Conf.Org
  ; heading_to_list = false
  ; exporting_keep_properties = false
  ; inline_type_with_pos = false
  ; export_md_indent_style = Conf.Dashes
  ; export_md_remove_options = []
  ; hiccup_in_block = true
  }

let check_mldoc_type =
  Alcotest.check (Alcotest.testable Type.pp ( = )) "check mldoc type"

let check_aux source expect =
  let result = Mldoc.Parser.parse default_config source |> List.hd |> fst in
  fun _ -> check_mldoc_type expect result

let testcases =
  List.map (fun (case, level, f) -> Alcotest.test_case case level f)

let paragraph l = Type.Paragraph (Type_op.inline_list_with_none_pos l)

let footnote_definition (s, l) =
  Type.Footnote_Definition (s, Type_op.inline_list_with_none_pos l)

let inline =
  let module I = Inline in
  [ ( "emphasis"
    , testcases
        [ ( "normal bold"
          , `Quick
          , check_aux "*a b c*"
              (paragraph [ I.Emphasis (`Bold, [ I.Plain "a b c" ]) ]) )
        ; ( "normal bold(2)"
          , `Quick
          , check_aux "a*b*c"
              (paragraph
                 [ I.Plain "a"
                 ; I.Emphasis (`Bold, [ I.Plain "b" ])
                 ; I.Plain "c"
                 ]) )
        ; ( "normal italic"
          , `Quick
          , check_aux "/a b c/"
              (paragraph [ I.Emphasis (`Italic, [ I.Plain "a b c" ]) ]) )
        ; ( "normal underline"
          , `Quick
          , check_aux "_a b c_"
              (paragraph [ I.Emphasis (`Underline, [ I.Plain "a b c" ]) ]) )
        ; ( "not emphasis (1)"
          , `Quick
          , check_aux "a * b*" (paragraph [ I.Plain "a * b*" ]) )
        ; ( "not emphasis (2)"
          , `Quick
          , check_aux "a_b_c"
              (paragraph [ I.Plain "a"; I.Subscript [ I.Plain "b_c" ] ]) )
        ; ( "contains underline"
          , `Quick
          , check_aux "_a _ a_"
              (paragraph [ I.Emphasis (`Underline, [ I.Plain "a _ a" ]) ]) )
        ; ( "contains star"
          , `Quick
          , check_aux "*a * a*"
              (paragraph [ I.Emphasis (`Bold, [ I.Plain "a * a" ]) ]) )
        ; ( "left flanking delimiter"
          , `Quick
          , check_aux "hello_world_"
              (paragraph [ I.Plain "hello"; I.Subscript [ I.Plain "world_" ] ])
          )
        ; ( "left flanking delimiter (2)"
          , `Quick
          , check_aux "hello,_world_"
              (paragraph
                 [ I.Plain "hello,"
                 ; I.Emphasis (`Underline, [ I.Plain "world" ])
                 ]) )
        ] )
  ; ( "inline-link"
    , testcases
        [ ( "normal"
          , `Quick
          , check_aux "[[http://example.com][[example] website]]"
              (paragraph
                 [ I.Link
                     { url =
                         I.Complex { protocol = "http"; link = "example.com" }
                     ; label = [ I.Plain "[example] website" ]
                     ; title = None
                     ; full_text = "[[http://example.com][[example] website]]"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "normal (2)"
          , `Quick
          , check_aux "[[http://example.com][[[example]] website]]"
              (paragraph
                 [ I.Link
                     { url =
                         I.Complex { protocol = "http"; link = "example.com" }
                     ; label = [ I.Plain "[[example]] website" ]
                     ; title = None
                     ; full_text = "[[http://example.com][[[example]] website]]"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "normal (3)"
          , `Quick
          , check_aux "[[http://example.com]]"
              (paragraph
                 [ I.Link
                     { url =
                         I.Complex { protocol = "http"; link = "example.com" }
                     ; label = [ I.Plain "http://example.com" ]
                     ; title = None
                     ; full_text = "[[http://example.com]]"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "normal (4)"
          , `Quick
          , check_aux "[[example]]"
              (paragraph
                 [ I.Link
                     { url = I.Page_ref "example"
                     ; label = [ I.Plain "" ]
                     ; title = None
                     ; full_text = "[[example]]"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "normal (5)"
          , `Quick
          , check_aux "[[exam:ple]]"
              (paragraph
                 [ I.Link
                     { url = I.Page_ref "exam:ple"
                     ; label = [ I.Plain "" ]
                     ; title = None
                     ; full_text = "[[exam:ple]]"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "normal (6)"
          , `Quick
          , check_aux "[[exam:ple][label]]"
              (paragraph
                 [ I.Link
                     { url = I.Complex { protocol = "exam"; link = "ple" }
                     ; label = [ I.Plain "label" ]
                     ; title = None
                     ; full_text = "[[exam:ple][label]]"
                     ; metadata = ""
                     }
                 ]) )
        ] )
  ]

let block =
  let open Type in
  let module I = Inline in
  [ ( "footnote-definition"
    , testcases
        [ ( "normal"
          , `Quick
          , check_aux "[fn:abc] 中文"
              (footnote_definition ("abc", [ I.Plain "中文" ])) )
        ] )
  ; ( "quote"
    , testcases
        [ ( "multi lines"
          , `Quick
          , check_aux "#+BEGIN_QUOTE\nfoo\nbar\n#+END_QUOTE"
              (Quote
                 [ paragraph
                     [ I.Plain "foo"
                     ; I.Break_Line
                     ; I.Plain "bar"
                     ; I.Break_Line
                     ]
                 ]) )
        ] )
  ; ( "example"
    , testcases
        [ ( "multi lines"
          , `Quick
          , check_aux "#+BEGIN_EXAMPLE\nfoo\nbar\n#+END_EXAMPLE"
              (Example [ "foo"; "\n"; "bar"; "\n" ]) )
        ] )
  ; ( "drawer"
    , testcases
        [ ( "properties"
          , `Quick
          , check_aux
              ":PROPERTIES:\n:XXX: 1\n:yyy: 2\n:END:\n#+ZZZ: 3\n#+UUU: 4"
              (Property_Drawer
                 [ ("XXX", "1"); ("yyy", "2"); ("ZZZ", "3"); ("UUU", "4") ]) )
        ; ( "no drawer in quote"
          , `Quick
          , check_aux "#+BEGIN_QUOTE\na:: b\n#+END_QUOTE"
              (Quote [ paragraph [ I.Plain "a:: b"; I.Break_Line ] ]) )
        ] )
  ; ( "src"
    , testcases
        [ ( "src with header arguments"
          , `Quick
          , check_aux
              "#+BEGIN_SRC haskell :results silent :exports code :var n=0\n\
              \  fac 0 = 1\n\
              \  fac n = n * fac (n-1)\n\
               #+END_SRC"
              (Type.Src
                 { lines = [ "fac 0 = 1"; "\n"; "fac n = n * fac (n-1)"; "\n" ]
                 ; language = Some "haskell"
                 ; options =
                     Some
                       [ ":results"
                       ; "silent"
                       ; ":exports"
                       ; "code"
                       ; ":var"
                       ; "n=0"
                       ]
                 ; pos_meta = { start_pos = 59; end_pos = 95 }
                 }) )
        ] )
  ; ( "Headline with tags"
    , testcases
        [ ( "(1)"
          , `Quick
          , check_aux "* aaa     :bb:cc:"
              (Type.Heading
                 { title = [ (I.Plain "aaa ", None) ]
                 ; tags = [ "bb"; "cc" ]
                 ; marker = None
                 ; level = 1
                 ; numbering = None
                 ; priority = None
                 ; anchor = "aaa"
                 ; meta = { Type.timestamps = []; properties = [] }
                 ; unordered = true
                 ; size = None
                 }) )
        ; ( "(2)"
          , `Quick
          , check_aux "* aaa [[link][label]]     :bb:cc:"
              (Type.Heading
                 { title =
                     [ (I.Plain "aaa ", None)
                     ; ( I.Link
                           { I.url = I.Search "link"
                           ; label = [ I.Plain "label" ]
                           ; title = None
                           ; full_text = "[[link][label]]"
                           ; metadata = ""
                           }
                       , None )
                     ]
                 ; tags = [ "bb"; "cc" ]
                 ; marker = None
                 ; level = 1
                 ; numbering = None
                 ; priority = None
                 ; anchor = "aaa_label"
                 ; meta = { Type.timestamps = []; properties = [] }
                 ; unordered = true
                 ; size = None
                 }) )
        ] )
  ]

let () = Alcotest.run "mldoc" @@ List.concat [ block; inline ]

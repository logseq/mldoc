let default_config : Conf.t =
  { toc = true
  ; heading_number = true
  ; keep_line_break = false
  ; format = Conf.Org
  ; heading_to_list = false
  ; exporting_keep_properties = false
  ; ignore_heading_list_marker = false
  }

let check_mldoc_type =
  Alcotest.check (Alcotest.testable Type.pp ( = )) "check mldoc type"

let check_aux source expect =
  let result = Mldoc.Parser.parse default_config source |> List.hd |> fst in
  fun _ -> check_mldoc_type expect result

let testcases =
  List.map (fun (case, level, f) -> Alcotest.test_case case level f)

let inline =
  let open Type in
  let module I = Inline in
  [ ( "emphasis"
    , testcases
        [ ( "normal bold"
          , `Quick
          , check_aux "*a b c*"
              (Paragraph [ I.Emphasis (`Bold, [ I.Plain "a b c" ]) ]) )
        ; ( "normal bold(2)"
          , `Quick
          , check_aux "a*b*c"
              (Paragraph
                 [ I.Plain "a"
                 ; I.Emphasis (`Bold, [ I.Plain "b" ])
                 ; I.Plain "c"
                 ]) )
        ; ( "normal italic"
          , `Quick
          , check_aux "/a b c/"
              (Paragraph [ I.Emphasis (`Italic, [ I.Plain "a b c" ]) ]) )
        ; ( "normal underline"
          , `Quick
          , check_aux "_a b c_"
              (Paragraph [ I.Emphasis (`Underline, [ I.Plain "a b c" ]) ]) )
        ; ( "not emphasis (1)"
          , `Quick
          , check_aux "a * b*" (Paragraph [ I.Plain "a * b*" ]) )
        ; ( "not emphasis (2)"
          , `Quick
          , check_aux "a_b_c"
              (Paragraph [ I.Plain "a"; I.Subscript [ I.Plain "b_c" ] ]) )
        ; ( "contains underline"
          , `Quick
          , check_aux "_a _ a_"
              (Paragraph [ I.Emphasis (`Underline, [ I.Plain "a _ a" ]) ]) )
        ; ( "contains star"
          , `Quick
          , check_aux "*a * a*"
              (Paragraph [ I.Emphasis (`Bold, [ I.Plain "a * a" ]) ]) )
        ; ( "left flanking delimiter"
          , `Quick
          , check_aux "hello_world_"
              (Paragraph [ I.Plain "hello"; I.Subscript [ I.Plain "world_" ] ])
          )
        ; ( "left flanking delimiter (2)"
          , `Quick
          , check_aux "hello,_world_"
              (Paragraph
                 [ I.Plain "hello,"
                 ; I.Emphasis (`Underline, [ I.Plain "world" ])
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
              (Footnote_Definition ("abc", [ I.Plain "中文" ])) )
        ] )
  ; ( "quote"
    , testcases
        [ ( "multi lines"
          , `Quick
          , check_aux "#+BEGIN_QUOTE\nfoo\nbar\n#+END_QUOTE"
              (Quote
                 [ Paragraph
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
  ]

let () = Alcotest.run "mldoc" @@ List.concat [ block; inline ]

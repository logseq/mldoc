let default_config : Conf.t =
  { toc = true
  ; heading_number = true
  ; keep_line_break = false
  ; format = Conf.Markdown
  ; heading_to_list = false
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
  [ ( "inline-link"
    , testcases
        [ ( "normal"
          , `Quick
          , check_aux "http://testtest/asdasd"
              (Paragraph
                 [ I.Link
                     { url =
                         I.Complex
                           { protocol = "http"; link = "//testtest/asdasd" }
                     ; label = [ Plain "http://testtest/asdasd" ]
                     ; title = None
                     ; full_text = "http://testtest/asdasd"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "normal-2"
          , `Quick
          , check_aux "[test  normal](http://testtest/asdasd)"
              (Paragraph
                 [ I.Link
                     { url =
                         I.Complex
                           { protocol = "http"; link = "//testtest/asdasd" }
                     ; label = [ Plain "test  normal" ]
                     ; title = None
                     ; full_text = "[test  normal](http://testtest/asdasd)"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "inline-code-in-label"
          , `Quick
          , check_aux "[test `normal`](http://testtest/asdasd)"
              (Paragraph
                 [ I.Link
                     { url =
                         I.Complex
                           { protocol = "http"; link = "//testtest/asdasd" }
                     ; label = [ Plain "test "; Code "normal" ]
                     ; title = None
                     ; full_text = "[test `normal`](http://testtest/asdasd)"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "with-*"
          , `Quick
          , check_aux "http://testtest/asd*asd"
              (Paragraph
                 [ I.Link
                     { url =
                         I.Complex
                           { protocol = "http"; link = "//testtest/asd*asd" }
                     ; label = [ Plain "http://testtest/asd*asd" ]
                     ; title = None
                     ; full_text = "http://testtest/asd*asd"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "spaces-around-link-line"
          , `Quick
          , check_aux " http://testtest/asdasd "
              (Paragraph
                 [ I.Plain " "
                 ; I.Link
                     { url =
                         I.Complex
                           { protocol = "http"; link = "//testtest/asdasd" }
                     ; label = [ Plain "http://testtest/asdasd" ]
                     ; title = None
                     ; full_text = "http://testtest/asdasd"
                     ; metadata = ""
                     }
                 ; I.Plain " "
                 ]) )
        ] )
  ; ( "inline-macro"
    , testcases
        [ ( "double"
          , `Quick
          , check_aux "{{test foo}}"
              (Paragraph
                 [ I.Macro { I.Macro.name = "test"; arguments = [ "foo" ] } ])
          )
        ; ( "three"
          , `Quick
          , check_aux "{{{test foo}}}"
              (Paragraph
                 [ I.Macro { I.Macro.name = "test"; arguments = [ "foo" ] } ])
          )
        ; ( "embed"
          , `Quick
          , check_aux "{{{embed [[page]]}}}"
              (Paragraph
                 [ I.Macro
                     { I.Macro.name = "embed"; arguments = [ "[[page]]" ] }
                 ]) )
        ; ( "args"
          , `Quick
          , check_aux "{{macroname [[A,B]], ((C,D)), E}}"
              (Paragraph
                 [ I.Macro
                     { I.Macro.name = "macroname"
                     ; arguments = [ "[[A,B]]"; "((C,D))"; "E" ]
                     }
                 ]) )
        ; ( "args-2"
          , `Quick
          , check_aux "{{macroname ([[A,B]], ((C,D)), E)}}"
              (Paragraph
                 [ I.Macro
                     { I.Macro.name = "macroname"
                     ; arguments = [ "[[A,B]]"; "((C,D))"; "E" ]
                     }
                 ]) )
        ] )
  ; ( "drawer"
    , testcases
        [ ( "empty-property-value"
          , `Quick
          , check_aux
              ":PROPERTIES:\n:type: programming_lang\n:creator: test\n:END:"
              (Property_Drawer
                 [ ("type", "programming_lang"); ("creator", "test") ]) )
        ; ( "spaces-before-drawer"
          , `Quick
          , check_aux
              " :PROPERTIES:\n:type: programming_lang\n:creator: test\n:END:"
              (Property_Drawer
                 [ ("type", "programming_lang"); ("creator", "test") ]) )
        ; ( "endwith-carriage-return"
          , `Quick
          , check_aux
              ":PROPERTIES:\r\n\
               :now: 1614485729874\n\
               :done: 1614485743195\r\n\
               :END:\n"
              (Property_Drawer
                 [ ("now", "1614485729874"); ("done", "1614485743195") ]) )
        ; ( "endwith-carriage-return-2"
          , `Quick
          , check_aux
              ":PROPERTIES:\r\n\
               :now: 1614485729874\r\n\
               :done: 1614485743195\r\n\
               :END:\r\n"
              (Property_Drawer
                 [ ("now", "1614485729874"); ("done", "1614485743195") ]) )
        ] )
  ; ( "inline-code"
    , testcases
        [ ( "normal"
          , `Quick
          , check_aux "`codes here`" (Paragraph [ I.Code "codes here" ]) )
        ; ( "overlap-with-emphasis"
          , `Quick
          , check_aux "*aa`*`" (Paragraph [ I.Plain "*aa"; I.Code "*" ]) )
        ; ( "overlap-with-emphasis-2"
          , `Quick
          , check_aux "**aa`**`" (Paragraph [ I.Plain "**aa"; I.Code "**" ]) )
        ; ( "overlap-with-emphasis-3"
          , `Quick
          , check_aux "_a`_`" (Paragraph [ I.Plain "_a"; I.Code "_" ]) )
        ; ( "overlap-with-emphasis-4"
          , `Quick
          , check_aux "__a`__`" (Paragraph [ I.Plain "__a"; I.Code "__" ]) )
        ; ( "overlap-with-emphasis-5"
          , `Quick
          , check_aux "`as*d`*" (Paragraph [ I.Code "as*d"; I.Plain "*" ]) )
        ; ( "overlap-with-link"
          , `Quick
          , check_aux "[as`d](`http://dwdw)"
              (Paragraph
                 [ I.Plain "[as"
                 ; I.Code "d]("
                 ; I.Link
                     { url = Complex { protocol = "http"; link = "//dwdw" }
                     ; label = [ Plain "http://dwdw" ]
                     ; title = None
                     ; full_text = "http://dwdw"
                     ; metadata = ""
                     }
                 ; I.Plain ")"
                 ]) )
        ; ( "overlap-with-link-2"
          , `Quick
          , check_aux "[as`d](http://dwdw)`"
              (Paragraph [ I.Plain "[as"; I.Code "d](http://dwdw)" ]) )
        ] )
  ; ( "emphasis"
    , testcases
        [ ( "normal"
          , `Quick
          , check_aux "*abc*"
              (Paragraph [ I.Emphasis (`Italic, [ Plain "abc" ]) ]) )
        ; ( "normal-2"
          , `Quick
          , check_aux "**abc**"
              (Paragraph [ I.Emphasis (`Bold, [ Plain "abc" ]) ]) )
        ; ( "inline-code-inside"
          , `Quick
          , check_aux "*asd`qwe`*"
              (Paragraph
                 [ I.Emphasis (`Italic, [ I.Plain "asd"; I.Code "qwe" ]) ]) )
        ; ( "inline-code-inside-2"
          , `Quick
          , check_aux "***asd`qwe`***"
              (Paragraph
                 [ I.Emphasis
                     ( `Italic
                     , [ I.Emphasis (`Bold, [ I.Plain "asd"; I.Code "qwe" ]) ]
                     )
                 ]) )
        ] )
  ; ( "tag"
    , testcases
        [ ( "endwith '.'"
          , `Quick
          , check_aux "#tag." (Paragraph [ I.Tag "tag"; I.Plain "." ]) )
        ; ( "endwith ','"
          , `Quick
          , check_aux "#tag," (Paragraph [ I.Tag "tag"; I.Plain "," ]) )
        ; ("with '.'", `Quick, check_aux "#a.b.c" (Paragraph [ I.Tag "a.b.c" ]))
        ; ( "with '.' and endwith '.'"
          , `Quick
          , check_aux "#a.b.c." (Paragraph [ I.Tag "a.b.c"; I.Plain "." ]) )
        ; ( "with '.' and endwith '.' (2)"
          , `Quick
          , check_aux "#a.b.c. defg"
              (Paragraph [ I.Tag "a.b.c"; I.Plain ". defg" ]) )
        ; ( "with page-ref"
          , `Quick
          , check_aux "#a.[[b c d ]].e."
              (Paragraph [ I.Tag "a.[[b c d ]].e"; I.Plain "." ]) )
        ] )
  ]

let block =
  let open Type in
  let module I = Inline in
  [ ( "footnote-definition"
    , testcases
        [ ( "normal"
          , `Quick
          , check_aux "[^abc]: 中文"
              (Footnote_Definition ("abc", [ I.Plain "中文" ])) )
        ] )
  ; ( "quote"
    , testcases
        [ ( "multi lines"
          , `Quick
          , check_aux ">foo\n>bar"
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
          , check_aux "    foo\n    bar" (Example [ "foo\n"; "bar\n" ]) )
        ] )
  ; ( "latex_env"
    , testcases
        [ ( "one-line"
          , `Quick
          , check_aux "\\begin{equation}[a,b,c] x=\\sqrt{b} \\end{equation}"
              (Latex_Environment ("equation", None, "[a,b,c] x=\\sqrt{b} ")) )
        ] )
  ]

let () = Alcotest.run "mldoc" @@ List.concat [ inline; block ]

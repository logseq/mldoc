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
  }

let check_mldoc_type =
  Alcotest.check (Alcotest.testable Type.pp ( = )) "check mldoc type"

let check_aux source expect =
  let result = Mldoc.Parser.parse default_config source |> List.hd |> fst in
  fun _ -> check_mldoc_type expect result

let check_mldoc_type2 =
  Alcotest.check
    (Alcotest.testable
       (fun fmt l -> List.map (Type.pp fmt) l |> ignore)
       (fun a b ->
         if List.length a <> List.length b then
           false
         else
           List.map2 ( = ) a b |> List.memq false |> not))
    "check mldoc type"

let check_aux2 source expect =
  let result = List.map fst (Mldoc.Parser.parse default_config source) in
  fun _ -> check_mldoc_type2 expect result

let testcases =
  List.map (fun (case, level, f) -> Alcotest.test_case case level f)

let paragraph l = Type.Paragraph (Type_op.inline_list_with_none_pos l)

let footnote_definition (s, l) =
  Type.Footnote_Definition (s, Type_op.inline_list_with_none_pos l)

let inline =
  let open Type in
  let module I = Inline in
  [ ( "inline-link"
    , testcases
        [ ( "normal"
          , `Quick
          , check_aux "http://testtest/asdasd"
              (paragraph
                 [ I.Link
                     { url =
                         I.Complex
                           { protocol = "http"; link = "testtest/asdasd" }
                     ; label = [ Plain "http://testtest/asdasd" ]
                     ; title = None
                     ; full_text = "http://testtest/asdasd"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "normal-2"
          , `Quick
          , check_aux "[test  normal](http://testtest/asdasd)"
              (paragraph
                 [ I.Link
                     { url =
                         I.Complex
                           { protocol = "http"; link = "testtest/asdasd" }
                     ; label = [ Plain "test  normal" ]
                     ; title = None
                     ; full_text = "[test  normal](http://testtest/asdasd)"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "inline-code-in-label"
          , `Quick
          , check_aux "[test `normal`](http://testtest/asdasd)"
              (paragraph
                 [ I.Link
                     { url =
                         I.Complex
                           { protocol = "http"; link = "testtest/asdasd" }
                     ; label = [ Plain "test "; Code "normal" ]
                     ; title = None
                     ; full_text = "[test `normal`](http://testtest/asdasd)"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "with-*"
          , `Quick
          , check_aux "http://testtest/asd*asd"
              (paragraph
                 [ I.Link
                     { url =
                         I.Complex
                           { protocol = "http"; link = "testtest/asd*asd" }
                     ; label = [ Plain "http://testtest/asd*asd" ]
                     ; title = None
                     ; full_text = "http://testtest/asd*asd"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "spaces-around-link-line"
          , `Quick
          , check_aux " http://testtest/asdasd "
              (paragraph
                 [ I.Plain " "
                 ; I.Link
                     { url =
                         I.Complex
                           { protocol = "http"; link = "testtest/asdasd" }
                     ; label = [ Plain "http://testtest/asdasd" ]
                     ; title = None
                     ; full_text = "http://testtest/asdasd"
                     ; metadata = ""
                     }
                 ; I.Plain " "
                 ]) )
        ; ( "endwith '.'"
          , `Quick
          , check_aux "http://test/f.o.o/b.a.r. "
              (paragraph
                 [ I.Link
                     { url =
                         I.Complex
                           { protocol = "http"; link = "test/f.o.o/b.a.r" }
                     ; label = [ Plain "http://test/f.o.o/b.a.r" ]
                     ; title = None
                     ; full_text = "http://test/f.o.o/b.a.r"
                     ; metadata = ""
                     }
                 ; I.Plain ". "
                 ]) )
        ; ( "include brackets"
          , `Quick
          , check_aux "http://test/(foo)bar"
              (paragraph
                 [ I.Link
                     { url =
                         I.Complex { protocol = "http"; link = "test/(foo)bar" }
                     ; label = [ Plain "http://test/(foo)bar" ]
                     ; title = None
                     ; full_text = "http://test/(foo)bar"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "include brackets (2)"
          , `Quick
          , check_aux "http://test/[(foo)b]ar"
              (paragraph
                 [ I.Link
                     { url =
                         I.Complex
                           { protocol = "http"; link = "test/[(foo)b]ar" }
                     ; label = [ Plain "http://test/[(foo)b]ar" ]
                     ; title = None
                     ; full_text = "http://test/[(foo)b]ar"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "include brackets (3)"
          , `Quick
          , check_aux "http://test/[foo)b]ar"
              (paragraph
                 [ I.Link
                     { url = I.Complex { protocol = "http"; link = "test/[foo" }
                     ; label = [ Plain "http://test/[foo" ]
                     ; title = None
                     ; full_text = "http://test/[foo"
                     ; metadata = ""
                     }
                 ; I.Plain ")b]ar"
                 ]) )
        ; ( "include brackets (4)"
          , `Quick
          , check_aux "http://te(s)t/foobar"
              (paragraph
                 [ I.Link
                     { url = I.Complex { protocol = "http"; link = "te" }
                     ; label = [ Plain "http://te" ]
                     ; title = None
                     ; full_text = "http://te"
                     ; metadata = ""
                     }
                 ; I.Plain "(s)t/foobar"
                 ]) )
        ] )
  ; ( "link"
    , testcases
        [ ( "normal"
          , `Quick
          , check_aux "[label here](http://foobar/path?query=123)"
              (paragraph
                 [ I.Link
                     { url =
                         I.Complex
                           { protocol = "http"; link = "foobar/path?query=123" }
                     ; label = [ Plain "label here" ]
                     ; title = None
                     ; full_text = "[label here](http://foobar/path?query=123)"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "also support org-link syntax"
          , `Quick
          , check_aux "[[http://foobar/path?query=123][label here]]"
              (paragraph
                 [ I.Link
                     { url =
                         I.Complex
                           { protocol = "http"; link = "foobar/path?query=123" }
                     ; label = [ Plain "label here" ]
                     ; title = None
                     ; full_text =
                         "[[http://foobar/path?query=123][label here]]"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "label with page-ref"
          , `Quick
          , check_aux "[abc [[d ef]] gh](../assets/0000.pdf)"
              (paragraph
                 [ I.Link
                     { url = I.Search "../assets/0000.pdf"
                     ; label = [ Plain "abc [[d ef]] gh" ]
                     ; title = None
                     ; full_text = "[abc [[d ef]] gh](../assets/0000.pdf)"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "with title"
          , `Quick
          , check_aux "[abc [[d]( ef]] gh](../assets/0000.pdf \"title\")"
              (paragraph
                 [ I.Link
                     { url = I.Search "../assets/0000.pdf"
                     ; label = [ Plain "abc [[d]( ef]] gh" ]
                     ; title = Some "title"
                     ; full_text =
                         "[abc [[d]( ef]] gh](../assets/0000.pdf \"title\")"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "include brackets"
          , `Quick
          , check_aux "[label](abc(def)gh)"
              (paragraph
                 [ I.Link
                     { url = I.Search "abc(def)gh"
                     ; label = [ Plain "label" ]
                     ; title = None
                     ; full_text = "[label](abc(def)gh)"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "include brackets (2)"
          , `Quick
          , check_aux "[中文](https://a.b.c.d/e/f%20g(1).h)"
              (paragraph
                 [ I.Link
                     { url =
                         I.Complex
                           { protocol = "https"; link = "a.b.c.d/e/f%20g(1).h" }
                     ; label = [ Plain "中文" ]
                     ; title = None
                     ; full_text = "[中文](https://a.b.c.d/e/f%20g(1).h)"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "page-ref before link"
          , `Quick
          , check_aux "[[a]][b](c)"
              (paragraph
                 [ I.Link
                     { url = I.Page_ref "a"
                     ; label = [ Plain "" ]
                     ; title = None
                     ; full_text = "[[a]]"
                     ; metadata = ""
                     }
                 ; I.Link
                     { url = I.Search "c"
                     ; label = [ Plain "b" ]
                     ; title = None
                     ; full_text = "[b](c)"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "url and title"
          , `Quick
          , check_aux "[a](bbb[[ccc \"dd\"]] \"e f\")"
              (paragraph
                 [ I.Link
                     { url = I.Search "bbb[[ccc \"dd\"]]"
                     ; label = [ Plain "a" ]
                     ; title = Some "e f"
                     ; full_text = "[a](bbb[[ccc \"dd\"]] \"e f\")"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "url part include page ref"
          , `Quick
          , check_aux "[a](bbb[[ccc \"dd\"]][[ff gg hh]] \"ee\")"
              (paragraph
                 [ I.Link
                     { url = I.Search "bbb[[ccc \"dd\"]][[ff gg hh]]"
                     ; label = [ Plain "a" ]
                     ; title = Some "ee"
                     ; full_text = "[a](bbb[[ccc \"dd\"]][[ff gg hh]] \"ee\")"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "page ref with label"
          , `Quick
          , check_aux "[label]([[page-ref]])"
              (paragraph
                 [ I.Link
                     { url = I.Page_ref "page-ref"
                     ; label = [ Plain "label" ]
                     ; title = None
                     ; full_text = "[label]([[page-ref]])"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "image link"
          , `Quick
          , check_aux "![lab[el]](url-part)"
              (paragraph
                 [ I.Link
                     { url = I.Search "url-part"
                     ; label = [ Plain "lab[el]" ]
                     ; title = None
                     ; full_text = "![lab[el]](url-part)"
                     ; metadata = ""
                     }
                 ]) )
        ] )
  ; ( "inline-macro"
    , testcases
        [ ( "double"
          , `Quick
          , check_aux "{{test foo}}"
              (paragraph
                 [ I.Macro { I.Macro.name = "test"; arguments = [ "foo" ] } ])
          )
        ; ( "three"
          , `Quick
          , check_aux "{{{test foo}}}"
              (paragraph
                 [ I.Macro { I.Macro.name = "test"; arguments = [ "foo" ] } ])
          )
        ; ( "query"
          , `Quick
          , check_aux "{{query (and [[test]])}}"
              (paragraph
                 [ I.Macro
                     { I.Macro.name = "query"
                     ; arguments = [ "(and [[test]])" ]
                     }
                 ]) )
        ; ( "embed"
          , `Quick
          , check_aux "{{{embed [[page]]}}}"
              (paragraph
                 [ I.Macro
                     { I.Macro.name = "embed"; arguments = [ "[[page]]" ] }
                 ]) )
        ; ( "query nested link"
          , `Quick
          , check_aux "{{{query [[page [[nested]]]]}}}"
              (paragraph
                 [ I.Macro
                     { I.Macro.name = "query"
                     ; arguments = [ "[[page [[nested]]]]" ]
                     }
                 ]) )
        ; ( "args"
          , `Quick
          , check_aux "{{macroname [[A,B]], ((C,D)), E}}"
              (paragraph
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
        ; ( "simplified-property-syntax"
          , `Quick
          , check_aux "a.b.c:: def\na-b-c::"
              (Property_Drawer [ ("a.b.c", "def"); ("a-b-c", "") ]) )
        ; ( "empty-property"
          , `Quick
          , check_aux ":PROPERTIES:\r\n:END:\r\n" (Property_Drawer []) )
        ; ( "no drawer in quote"
          , `Quick
          , check_aux "> a:: b"
              (Quote [ paragraph [ I.Plain "a:: b"; I.Break_Line ] ]) )
        ] )
  ; ( "inline-code"
    , testcases
        [ ( "normal"
          , `Quick
          , check_aux "`codes here`" (paragraph [ I.Code "codes here" ]) )
        ; ( "overlap-with-emphasis"
          , `Quick
          , check_aux "*aa`*`" (paragraph [ I.Plain "*aa"; I.Code "*" ]) )
        ; ( "overlap-with-emphasis-2"
          , `Quick
          , check_aux "**aa`**`" (paragraph [ I.Plain "**aa"; I.Code "**" ]) )
        ; ( "overlap-with-emphasis-3"
          , `Quick
          , check_aux "_a`_`" (paragraph [ I.Plain "_a"; I.Code "_" ]) )
        ; ( "overlap-with-emphasis-4"
          , `Quick
          , check_aux "__a`__`" (paragraph [ I.Plain "__a"; I.Code "__" ]) )
        ; ( "overlap-with-emphasis-5"
          , `Quick
          , check_aux "`as*d`*" (paragraph [ I.Code "as*d"; I.Plain "*" ]) )
        ; ( "overlap-with-link"
          , `Quick
          , check_aux "[as`d](`http://dwdw)"
              (paragraph
                 [ I.Plain "[as"
                 ; I.Code "d]("
                 ; I.Link
                     { url = Complex { protocol = "http"; link = "dwdw" }
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
              (paragraph [ I.Plain "[as"; I.Code "d](http://dwdw)" ]) )
        ] )
  ; ( "emphasis"
    , testcases
        [ ( "normal"
          , `Quick
          , check_aux "*abc*"
              (paragraph [ I.Emphasis (`Italic, [ Plain "abc" ]) ]) )
        ; ( "normal-2"
          , `Quick
          , check_aux "**abc**"
              (paragraph [ I.Emphasis (`Bold, [ Plain "abc" ]) ]) )
        ; ( "normal-3"
          , `Quick
          , check_aux "_a_,"
              (paragraph [ I.Emphasis (`Italic, [ Plain "a" ]); I.Plain "," ])
          )
        ; ( "inline-code-inside"
          , `Quick
          , check_aux "*asd`qwe`*"
              (paragraph
                 [ I.Emphasis (`Italic, [ I.Plain "asd"; I.Code "qwe" ]) ]) )
        ; ( "inline-code-inside-2"
          , `Quick
          , check_aux "***asd`qwe`***"
              (paragraph
                 [ I.Emphasis
                     ( `Italic
                     , [ I.Emphasis (`Bold, [ I.Plain "asd"; I.Code "qwe" ]) ]
                     )
                 ]) )
        ; ( "not emphasis (1)"
          , `Quick
          , check_aux "a * b*" (paragraph [ I.Plain "a * b*" ]) )
        ; ( "not emphasis (2)"
          , `Quick
          , check_aux "a_b_c" (paragraph [ I.Plain "a_b_c" ]) )
        ; ( "contains underline"
          , `Quick
          , check_aux "_a _ a_"
              (paragraph [ I.Emphasis (`Italic, [ I.Plain "a _ a" ]) ]) )
        ; ( "contains star"
          , `Quick
          , check_aux "*a * a*"
              (paragraph [ I.Emphasis (`Italic, [ I.Plain "a * a" ]) ]) )
        ; ( "left flanking delimiter"
          , `Quick
          , check_aux "hello_world_" (paragraph [ I.Plain "hello_world_" ]) )
        ; ( "left flanking delimiter (2)"
          , `Quick
          , check_aux "hello,_world_"
              (paragraph
                 [ I.Plain "hello,"; I.Emphasis (`Italic, [ I.Plain "world" ]) ])
          )
        ; ( "highlight (1)"
          , `Quick
          , check_aux "111==text==222"
              (paragraph
                 [ I.Plain "111"
                 ; I.Emphasis (`Highlight, [ I.Plain "text" ])
                 ; I.Plain "222"
                 ]) )
        ; ( "highlight (2)"
          , `Quick
          , check_aux "111== text==222"
              (paragraph [ I.Plain "111== text==222" ]) )
        ] )
  ; ( "tag"
    , testcases
        [ ( "endwith '.'"
          , `Quick
          , check_aux "#tag."
              (paragraph [ I.Tag [ I.Plain "tag" ]; I.Plain "." ]) )
        ; ( "endwith ','"
          , `Quick
          , check_aux "#tag,"
              (paragraph [ I.Tag [ I.Plain "tag" ]; I.Plain "," ]) )
        ; ( "endwith '\"'"
          , `Quick
          , check_aux "#tag\""
              (paragraph [ I.Tag [ I.Plain "tag" ]; I.Plain "\"" ]) )
        ; ( "endwith several periods"
          , `Quick
          , check_aux "#tag,.?"
              (paragraph [ I.Tag [ I.Plain "tag" ]; I.Plain ",.?" ]) )
        ; ( "with '.'"
          , `Quick
          , check_aux "#a.b.c" (paragraph [ I.Tag [ I.Plain "a.b.c" ] ]) )
        ; ( "with '.' and endwith '.'"
          , `Quick
          , check_aux "#a.b.c."
              (paragraph [ I.Tag [ I.Plain "a.b.c" ]; I.Plain "." ]) )
        ; ( "with '.' and endwith '.' (2)"
          , `Quick
          , check_aux "#a.b.c. defg"
              (paragraph [ I.Tag [ I.Plain "a.b.c" ]; I.Plain ". defg" ]) )
        ; ( "with page-ref"
          , `Quick
          , check_aux "#a.[[b c d ]].e."
              (paragraph
                 [ I.Tag
                     [ I.Plain "a."
                     ; I.Link
                         { url = I.Page_ref "b c d "
                         ; label = [ I.Plain "" ]
                         ; full_text = "[[b c d ]]"
                         ; metadata = ""
                         ; title = None
                         }
                     ; I.Plain ".e"
                     ]
                 ; I.Plain "."
                 ]) )
        ] )
  ; ( "footnote-reference"
    , testcases
        [ ( "footnote ref before link"
          , `Quick
          , check_aux "[^1][label](url)"
              (paragraph
                 [ I.Footnote_Reference
                     { id = 1; name = "1"; definition = None }
                 ; I.Link
                     { url = I.Search "url"
                     ; label = [ I.Plain "label" ]
                     ; title = None
                     ; full_text = "[label](url)"
                     ; metadata = ""
                     }
                 ]) )
        ] )
  ; ( "escape metachars"
    , testcases
        [ ( "emphasis"
          , `Quick
          , check_aux "*a\\*b*"
              (paragraph [ Inline.Emphasis (`Italic, [ Inline.Plain "a*b" ]) ])
          )
        ; ( "code"
          , `Quick
          , check_aux "`a\\``"
              (paragraph [ Inline.Code "a\\"; Inline.Plain "`" ]) )
        ; ( "nested emphasis"
          , `Quick
          , check_aux "_a*b\\*_"
              (paragraph [ Inline.Emphasis (`Italic, [ Inline.Plain "a*b*" ]) ])
          )
        ; ( "link (1)"
          , `Quick
          , check_aux "[[\\]]]"
              (paragraph
                 [ Inline.Link
                     { url = Inline.Page_ref "]"
                     ; label = [ Inline.Plain "" ]
                     ; full_text = "[[\\]]]"
                     ; metadata = ""
                     ; title = None
                     }
                 ]) )
        ; ( "link (2)"
          , `Quick
          , check_aux "[label\\](x)](xxx)"
              (paragraph
                 [ Inline.Link
                     { url = Inline.Search "xxx"
                     ; label = [ Inline.Plain "label](x)" ]
                     ; full_text = "[label\\](x)](xxx)"
                     ; metadata = ""
                     ; title = None
                     }
                 ]) )
        ; ( "link (3)"
          , `Quick
          , check_aux "[label](ur\\)l)"
              (paragraph
                 [ Inline.Link
                     { url = Inline.Search "ur)l"
                     ; label = [ Inline.Plain "label" ]
                     ; full_text = "[label](ur\\)l)"
                     ; metadata = ""
                     ; title = None
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
          , check_aux "[^abc]: 中文"
              (footnote_definition ("abc", [ I.Plain "中文" ])) )
        ] )
  ; ( "quote"
    , testcases
        [ ( "multi lines"
          , `Quick
          , check_aux ">foo\n>bar"
              (Quote
                 [ paragraph
                     [ I.Plain "foo"
                     ; I.Break_Line
                     ; I.Plain "bar"
                     ; I.Break_Line
                     ]
                 ]) )
        ] )
  ; ( "latex_env"
    , testcases
        [ ( "one-line"
          , `Quick
          , check_aux "\\begin{equation}[a,b,c] x=\\sqrt{b} \\end{equation}"
              (Latex_Environment ("equation", None, "[a,b,c] x=\\sqrt{b} ")) )
        ] )
  ; ( "list"
    , testcases
        [ ( "heading in list"
          , `Quick
          , check_aux "+ line1\n  - heading"
              (List
                 [ { content = [ paragraph [ I.Plain "line1" ] ]
                   ; items = []
                   ; number = None
                   ; name = []
                   ; checkbox = None
                   ; indent = 0
                   ; ordered = false
                   }
                 ]) )
        ; ( "heading in list (2)"
          , `Quick
          , check_aux "+ line1\n  -"
              (List
                 [ { content = [ paragraph [ I.Plain "line1" ] ]
                   ; items = []
                   ; number = None
                   ; name = []
                   ; checkbox = None
                   ; indent = 0
                   ; ordered = false
                   }
                 ]) )
        ] )
  ; ( "unordered list"
    , testcases
        [ ( "with size (1)"
          , `Quick
          , check_aux "- ## TODO text"
              (Type.Heading
                 { Type.title =
                     Type_op.inline_list_with_none_pos [ Inline.Plain "text" ]
                 ; tags = []
                 ; marker = Some "TODO"
                 ; level = 1
                 ; numbering = None
                 ; priority = None
                 ; anchor = "text"
                 ; meta = { Type.timestamps = []; properties = [] }
                 ; unordered = true
                 ; size = Some 2
                 }) )
        ; ( "with size (2)"
          , `Quick
          , check_aux "- ##"
              (Type.Heading
                 { Type.title = []
                 ; tags = []
                 ; marker = None
                 ; level = 1
                 ; numbering = None
                 ; priority = None
                 ; anchor = ""
                 ; meta = { Type.timestamps = []; properties = [] }
                 ; unordered = true
                 ; size = Some 2
                 }) )
        ; ( "followed by #tag"
          , `Quick
          , check_aux "- #tag"
              (Type.Heading
                 { Type.title =
                     Type_op.inline_list_with_none_pos
                       [ Inline.Tag [ I.Plain "tag" ] ]
                 ; tags = []
                 ; marker = None
                 ; level = 1
                 ; numbering = None
                 ; priority = None
                 ; anchor = ""
                 ; meta = { Type.timestamps = []; properties = [] }
                 ; unordered = true
                 ; size = None
                 }) )
        ; ( "drawer"
          , `Quick
          , check_aux "a:: 1\n#+b: 2"
              (Type.Property_Drawer [ ("a", "1"); ("b", "2") ]) )
        ] )
  ; ( "code block"
    , testcases
        [ ( "(1)"
          , `Quick
          , check_aux2 "- ```\ncode\n```"
              [ Type.Heading
                  { title = []
                  ; tags = []
                  ; marker = None
                  ; level = 1
                  ; numbering = None
                  ; priority = None
                  ; anchor = ""
                  ; meta = { Type.timestamps = []; properties = [] }
                  ; unordered = true
                  ; size = None
                  }
              ; Type.Src
                  { lines = [ "code"; "\n" ]
                  ; language = None
                  ; options = None
                  ; pos_meta = { Pos.start_pos = 6; end_pos = 11 }
                  }
              ] )
        ] )
  ]

let () = Alcotest.run "mldoc" @@ List.concat [ inline; block ]

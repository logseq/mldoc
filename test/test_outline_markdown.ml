let default_config : Conf.t =
  { toc = true
  ; parse_outline_only = true
  ; heading_number = true
  ; keep_line_break = false
  ; format = Conf.Markdown
  ; heading_to_list = false
  ; exporting_keep_properties = false
  ; inline_type_with_pos = false
  ; inline_skip_macro = false
  ; export_md_indent_style = Conf.Dashes
  ; export_md_remove_options = []
  ; hiccup_in_block = true
  }

let check_mldoc_type =
  Alcotest.check (Alcotest.testable Type.pp ( = )) "check mldoc type"

let paragraph l = Type.Paragraph (Type_op.inline_list_with_none_pos l)

let check_aux source expect =
  let result = Mldoc.Parser.parse default_config source in
  let result =
    match result with
    | [] -> paragraph []
    | _ -> result |> List.hd |> fst
  in
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

let footnote_definition (s, l) =
  Type.Footnote_Definition (s, Type_op.inline_list_with_none_pos l)

let inline =
  (* let open Type in *)
  let module I = Inline in
  [ ( "inline-link"
    , testcases
        [ ("normal", `Quick, check_aux "http://testtest/asdasd" (paragraph []))
        ; ( "link with page alias"
          , `Quick
          , check_aux "[foo](bar)"
              (paragraph
                 [ I.Link
                     { url = I.Search "bar"
                     ; label = [ I.Plain "foo" ]
                     ; title = None
                     ; full_text = "[foo](bar)"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "link with [[page alias]]"
          , `Quick
          , check_aux "[foo]([[bar]])"
              (paragraph
                 [ I.Link
                     { url = I.Page_ref "bar"
                     ; label = [ I.Plain "foo" ]
                     ; title = None
                     ; full_text = "[foo]([[bar]])"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "include brackets"
          , `Quick
          , check_aux "http://test/(foo)bar" (paragraph []) )
        ; ( "include brackets (2)"
          , `Quick
          , check_aux "http://test/[(foo)b]ar" (paragraph []) )
        ; ( "include brackets (3)"
          , `Quick
          , check_aux "http://test/[foo)b]ar" (paragraph []) )
        ; ( "include brackets (4)"
          , `Quick
          , check_aux "http://te(s)t/foobar" (paragraph []) )
        ; ( "include brackets (5)"
          , `Quick
          , check_aux "http://test/foo{bar}" (paragraph []) )
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
        ; ( "normal (2)"
          , `Quick
          , check_aux "[not label][label](url)"
              (paragraph
                 [ I.Link
                     { url = I.Search "url"
                     ; label = [ Plain "label" ]
                     ; title = None
                     ; full_text = "[label](url)"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "normal (3)"
          , `Quick
          , check_aux "[[page:name]]"
              (paragraph
                 [ I.Link
                     { url = I.Page_ref "page:name"
                     ; label = [ Plain "" ]
                     ; title = None
                     ; full_text = "[[page:name]]"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "normal (4)"
          , `Quick
          , check_aux "[[page://name]]"
              (paragraph
                 [ I.Link
                     { url = I.Page_ref "page://name"
                     ; label = [ Plain "" ]
                     ; title = None
                     ; full_text = "[[page://name]]"
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
        ; ( "url and title (1)"
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
        ; ( "url and title (2)"
          , `Quick
          , check_aux "[a](<bbb> \"cc\")"
              (paragraph
                 [ I.Link
                     { url = I.Search "bbb"
                     ; label = [ Plain "a" ]
                     ; title = Some "cc"
                     ; full_text = "[a](<bbb> \"cc\")"
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
        ; ( "page ref has []"
          , `Quick
          , check_aux "[[a[b]c]]"
              (paragraph
                 [ I.Link
                     { url = I.Page_ref "a[b]c"
                     ; label = [ Plain "" ]
                     ; title = None
                     ; full_text = "[[a[b]c]]"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "page ref has [] (2)"
          , `Quick
          , check_aux "[[a [b] c]]"
              (paragraph
                 [ I.Link
                     { url = I.Page_ref "a [b] c"
                     ; label = [ Plain "" ]
                     ; title = None
                     ; full_text = "[[a [b] c]]"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "image link will be treated as normal link"
          , `Quick
          , check_aux "![lab[el]](url-part)"
              (paragraph
                 [ I.Link
                     { url = I.Search "url-part"
                     ; label = [ Plain "lab[el]" ]
                     ; title = None
                     ; full_text = "[lab[el]](url-part)"
                     ; metadata = ""
                     }
                 ]) )
        ; ( "link url contains spaces"
          , `Quick
          , check_aux "[label](<u r l>)"
              (paragraph
                 [ I.Link
                     { url = I.Search "u r l"
                     ; label = [ Plain "label" ]
                     ; title = None
                     ; full_text = "[label](<u r l>)"
                     ; metadata = ""
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
                 [ ("type", "programming_lang", []); ("creator", "test", []) ]) )
        ; ( "spaces-before-drawer"
          , `Quick
          , check_aux
              " :PROPERTIES:\n:type: programming_lang\n:creator: test\n:END:"
              (Property_Drawer
                 [ ("type", "programming_lang", []); ("creator", "test", []) ]) )
        ; ( "endwith-carriage-return"
          , `Quick
          , check_aux
              ":PROPERTIES:\r\n\
               :now: 1614485729874\n\
               :done: 1614485743195\r\n\
               :END:\n"
              (Property_Drawer
                 [ ("now", "1614485729874", []); ("done", "1614485743195", []) ]) )
        ; ( "endwith-carriage-return-2"
          , `Quick
          , check_aux
              ":PROPERTIES:\r\n\
               :now: 1614485729874\r\n\
               :done: 1614485743195\r\n\
               :END:\r\n"
              (Property_Drawer
                 [ ("now", "1614485729874", []); ("done", "1614485743195", []) ]) )
        ; ( "simplified-property-syntax"
          , `Quick
          , check_aux "a.b.c:: def\na-b-c::"
              (Property_Drawer [ ("a.b.c", "def", []); ("a-b-c", "", []) ]) )
        ; ( "empty-property"
          , `Quick
          , check_aux ":PROPERTIES:\r\n:END:\r\n" (Property_Drawer []) )
        ; ( "no drawer in quote"
          , `Quick
          , check_aux "> a:: b" (Quote [ paragraph [] ]) )
        ] )
  ; ( "inline-code"
    , testcases
        [ ("normal", `Quick, check_aux "`codes here`" (paragraph []))
        ; ("overlap-with-emphasis", `Quick, check_aux "*aa`*`" (paragraph []))
        ; ( "overlap-with-emphasis-2"
          , `Quick
          , check_aux "**aa`**`" (paragraph []) )
        ; ("overlap-with-emphasis-3", `Quick, check_aux "_a`_`" (paragraph []))
        ; ("overlap-with-emphasis-4", `Quick, check_aux "__a`__`" (paragraph []))
        ; ("overlap-with-emphasis-5", `Quick, check_aux "`as*d`*" (paragraph []))
        ; ( "overlap-with-link"
          , `Quick
          , check_aux "[as`d](`http://dwdw)" (paragraph []) )
        ; ( "overlap-with-link-2"
          , `Quick
          , check_aux "[as`d](http://dwdw)`" (paragraph []) )
        ] )
  ; ( "emphasis"
    , testcases
        [ ("normal", `Quick, check_aux "*abc*" (paragraph []))
        ; ("normal-2", `Quick, check_aux "**abc**" (paragraph []))
        ; ("normal-3", `Quick, check_aux "_a_," (paragraph []))
        ; ("inline-code-inside", `Quick, check_aux "*asd`qwe`*" (paragraph []))
        ; ( "inline-code-inside-2"
          , `Quick
          , check_aux "***asd`qwe`***" (paragraph []) )
        ; ("not emphasis (1)", `Quick, check_aux "a * b*" (paragraph []))
        ; ("not emphasis (2)", `Quick, check_aux "a_b_c" (paragraph []))
        ; ("contains underline", `Quick, check_aux "_a _ a_" (paragraph []))
        ; ("contains star", `Quick, check_aux "*a * a*" (paragraph []))
        ; ( "left flanking delimiter"
          , `Quick
          , check_aux "hello_world_" (paragraph []) )
        ; ( "left flanking delimiter (2)"
          , `Quick
          , check_aux "hello,_world_" (paragraph []) )
        ; ("highlight (1)", `Quick, check_aux "111==text==222" (paragraph []))
        ; ("highlight (2)", `Quick, check_aux "111== text==222" (paragraph []))
        ] )
  ; ( "tag"
    , testcases
        [ ( "endwith '.'"
          , `Quick
          , check_aux "#tag." (paragraph [ I.Tag [ I.Plain "tag" ] ]) )
        ; ( "endwith ','"
          , `Quick
          , check_aux "#tag," (paragraph [ I.Tag [ I.Plain "tag" ] ]) )
        ; ( "endwith '\"'"
          , `Quick
          , check_aux "#tag\"" (paragraph [ I.Tag [ I.Plain "tag" ] ]) )
        ; ( "endwith several periods"
          , `Quick
          , check_aux "#tag,.?" (paragraph [ I.Tag [ I.Plain "tag" ] ]) )
        ; ( "with '.'"
          , `Quick
          , check_aux "#a.b.c" (paragraph [ I.Tag [ I.Plain "a.b.c" ] ]) )
        ; ( "with '.' and endwith '.'"
          , `Quick
          , check_aux "#a.b.c." (paragraph [ I.Tag [ I.Plain "a.b.c" ] ]) )
        ; ( "with '.' and endwith '.' (2)"
          , `Quick
          , check_aux "#a.b.c. defg" (paragraph [ I.Tag [ I.Plain "a.b.c" ] ])
          )
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
                 ]) )
        ] )
  ; ( "footnote-reference"
    , testcases
        [ ( "footnote ref before link"
          , `Quick
          , check_aux "[^1][label](url)"
              (paragraph
                 [ I.Link
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
        [ ("emphasis(1)", `Quick, check_aux "*a\\*b*" (paragraph []))
        ; ("emphasis(2)", `Quick, check_aux "*a\\\\\\*b*" (paragraph []))
        ; ("code", `Quick, check_aux "`a\\``" (paragraph []))
        ; ("nested emphasis", `Quick, check_aux "_a*b\\*_" (paragraph []))
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
  ; ( "Timestamps"
    , testcases
        [ ( "scheduled"
          , `Quick
          , check_aux "SCHEDULED: <2004-12-25 Sat>"
              (paragraph
                 [ I.Timestamp
                     (Scheduled
                        Timestamp.
                          { date = { year = 2004; month = 12; day = 25 }
                          ; wday = "Sat"
                          ; time = None
                          ; repetition = None
                          ; active = true
                          })
                 ]) )
        ; ( "scheduled with time"
          , `Quick
          , check_aux "SCHEDULED: <2004-12-25 Sat 10:00>"
              (paragraph
                 [ I.Timestamp
                     (Scheduled
                        Timestamp.
                          { date = { year = 2004; month = 12; day = 25 }
                          ; wday = "Sat"
                          ; time = Some { hour = 10; min = 0 }
                          ; repetition = None
                          ; active = true
                          })
                 ]) )
        ; ( "scheduled with a repeater"
          , `Quick
          , check_aux "SCHEDULED: <2004-12-25 Sat +1m>"
              (paragraph
                 [ I.Timestamp
                     (Scheduled
                        Timestamp.
                          { date = { year = 2004; month = 12; day = 25 }
                          ; wday = "Sat"
                          ; time = None
                          ; repetition = Some (Plus, Month, 1)
                          ; active = true
                          })
                 ]) )
        ; ( "scheduled after some text"
          , `Quick
          , check_aux "blabla SCHEDULED: <2004-12-25 Sat>"
              (paragraph
                 [ I.Timestamp
                     (Scheduled
                        Timestamp.
                          { date = { year = 2004; month = 12; day = 25 }
                          ; wday = "Sat"
                          ; time = None
                          ; repetition = None
                          ; active = true
                          })
                 ]) )
        ; ( "deadline"
          , `Quick
          , check_aux "DEADLINE: <2004-12-25 Sat>"
              (paragraph
                 [ I.Timestamp
                     (Deadline
                        Timestamp.
                          { date = { year = 2004; month = 12; day = 25 }
                          ; wday = "Sat"
                          ; time = None
                          ; repetition = None
                          ; active = true
                          })
                 ]) )
        ; ( "deadline with time"
          , `Quick
          , check_aux "DEADLINE: <2004-12-25 Sat 10:00>"
              (paragraph
                 [ I.Timestamp
                     (Deadline
                        Timestamp.
                          { date = { year = 2004; month = 12; day = 25 }
                          ; wday = "Sat"
                          ; time = Some { hour = 10; min = 0 }
                          ; repetition = None
                          ; active = true
                          })
                 ]) )
        ; ( "deadline with a repeater"
          , `Quick
          , check_aux "DEADLINE: <2004-12-25 Sat +1m>"
              (paragraph
                 [ I.Timestamp
                     (Deadline
                        Timestamp.
                          { date = { year = 2004; month = 12; day = 25 }
                          ; wday = "Sat"
                          ; time = None
                          ; repetition = Some (Plus, Month, 1)
                          ; active = true
                          })
                 ]) )
        ; ( "deadline after some text"
          , `Quick
          , check_aux "blabla DEADLINE: <2004-12-25 Sat>"
              (paragraph
                 [ I.Timestamp
                     (Deadline
                        Timestamp.
                          { date = { year = 2004; month = 12; day = 25 }
                          ; wday = "Sat"
                          ; time = None
                          ; repetition = None
                          ; active = true
                          })
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
          , check_aux ">foo\n>bar" (Quote [ paragraph [] ]) )
        ] )
  ; ( "latex_env"
    , testcases
        [ ( "one-line"
          , `Quick
          , check_aux "\\begin{equation}[a,b,c] x=\\sqrt{b} \\end{equation}"
              (paragraph []) )
        ] )
  ; ( "list"
    , testcases
        [ ( "heading in list"
          , `Quick
          , check_aux "+ line1\n  - heading"
              (List
                 [ { content = [ paragraph [] ]
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
                 [ { content = [ paragraph [] ]
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
                 { Type.title = []
                 ; tags = []
                 ; marker = Some "TODO"
                 ; level = 1
                 ; numbering = None
                 ; priority = None
                 ; anchor = ""
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
              (Type.Property_Drawer [ ("a", "1", []); ("b", "2", []) ]) )
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
              ] )
        ] )
  ; ( "list with timestamps"
    , testcases
        [ ( "a list with a scheduled"
          , `Quick
          , check_aux2 "- test\nSCHEDULED: <2004-12-25 Sat>"
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
              ; paragraph
                  [ I.Timestamp
                      (Scheduled
                         Timestamp.
                           { date = { year = 2004; month = 12; day = 25 }
                           ; wday = "Sat"
                           ; time = None
                           ; repetition = None
                           ; active = true
                           })
                  ]
              ] )
        ; ( "a heading with a scheduled"
          , `Quick
          , check_aux2 "# test\nSCHEDULED: <2004-12-25 Sat>"
              [ Type.Heading
                  { title = []
                  ; tags = []
                  ; marker = None
                  ; level = 1
                  ; numbering = None
                  ; priority = None
                  ; anchor = ""
                  ; meta = { Type.timestamps = []; properties = [] }
                  ; unordered = false
                  ; size = Some 1
                  }
              ; paragraph
                  [ I.Timestamp
                      (Scheduled
                         Timestamp.
                           { date = { year = 2004; month = 12; day = 25 }
                           ; wday = "Sat"
                           ; time = None
                           ; repetition = None
                           ; active = true
                           })
                  ]
              ] )
        ; ( "a heading with a scheduled and some text"
          , `Quick
          , check_aux2 "# test\nSCHEDULED: <2004-12-25 Sat>\nsome [[page]]"
              [ Type.Heading
                  { title = []
                  ; tags = []
                  ; marker = None
                  ; level = 1
                  ; numbering = None
                  ; priority = None
                  ; anchor = ""
                  ; meta = { Type.timestamps = []; properties = [] }
                  ; unordered = false
                  ; size = Some 1
                  }
              ; paragraph
                  [ I.Timestamp
                      (Scheduled
                         Timestamp.
                           { date = { year = 2004; month = 12; day = 25 }
                           ; wday = "Sat"
                           ; time = None
                           ; repetition = None
                           ; active = true
                           })
                  ; I.Link
                      { url = I.Page_ref "page"
                      ; label = [ I.Plain "" ]
                      ; title = None
                      ; full_text = "[[page]]"
                      ; metadata = ""
                      }
                  ]
              ] )
        ; ( "a heading with a scheduled, a deadline and some text"
          , `Quick
          , check_aux2
              "# test\n\
               SCHEDULED: <2004-12-25 Sat>\n\
               DEADLINE: <2004-12-25 Sat>\n\
               some [[page]]"
              [ Type.Heading
                  { title = []
                  ; tags = []
                  ; marker = None
                  ; level = 1
                  ; numbering = None
                  ; priority = None
                  ; anchor = ""
                  ; meta = { Type.timestamps = []; properties = [] }
                  ; unordered = false
                  ; size = Some 1
                  }
              ; paragraph
                  [ I.Timestamp
                      (Scheduled
                         Timestamp.
                           { date = { year = 2004; month = 12; day = 25 }
                           ; wday = "Sat"
                           ; time = None
                           ; repetition = None
                           ; active = true
                           })
                  ; I.Timestamp
                      (Deadline
                         Timestamp.
                           { date = { year = 2004; month = 12; day = 25 }
                           ; wday = "Sat"
                           ; time = None
                           ; repetition = None
                           ; active = true
                           })
                  ; I.Link
                      { url = I.Page_ref "page"
                      ; label = [ I.Plain "" ]
                      ; title = None
                      ; full_text = "[[page]]"
                      ; metadata = ""
                      }
                  ]
              ] )
        ] )
  ]

let () = Alcotest.run "mldoc" @@ List.concat [ inline; block ]

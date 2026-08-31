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
  ; enable_drawers = true
  ; parse_marker = true
  ; parse_priority = true
  }

let check_mldoc_type =
  Alcotest.check (Alcotest.testable Type.pp ( = )) "check mldoc type"

let paragraph l = Type.Paragraph (Type_op.inline_list_with_none_pos l)
let plain s = paragraph [ Inline.Plain s ]

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
        [ ( "normal"
          , `Quick
          , check_aux "http://testtest/asdasd" (plain "http://testtest/asdasd")
          )
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
          , check_aux "http://test/(foo)bar" (plain "http://test/(foo)bar") )
        ; ( "include brackets (2)"
          , `Quick
          , check_aux "http://test/[(foo)b]ar" (plain "http://test/[(foo)b]ar")
          )
        ; ( "include brackets (3)"
          , `Quick
          , check_aux "http://test/[foo)b]ar" (plain "http://test/[foo)b]ar") )
        ; ( "include brackets (4)"
          , `Quick
          , check_aux "http://te(s)t/foobar" (plain "http://te(s)t/foobar") )
        ; ( "include brackets (5)"
          , `Quick
          , check_aux "http://test/foo{bar}" (plain "http://test/foo{bar}") )
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
                 [ I.Plain "[not label]"
                 ; I.Link
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
                 [ I.Plain "!"
                 ; I.Link
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
                 [ ("type", "programming_lang", []); ("creator", "test", []) ])
          )
        ; ( "property-value-nested-ref"
          , `Quick
          , check_aux
              ":PROPERTIES:\n\
               :type: [[programming [[clojure]]]]\n\
               :creator: test\n\
               :END:"
              (Property_Drawer
                 [ ( "type"
                   , "[[programming [[clojure]]]]"
                   , [ I.Nested_link
                         { content = "[[programming [[clojure]]]]"
                         ; children =
                             [ Nested_link.Label "programming "
                             ; Nested_link.Nested_link
                                 ( { content = "[[clojure]]"
                                   ; children = [ Nested_link.Label "clojure" ]
                                   }
                                 , None )
                             ]
                         }
                     ] )
                 ; ("creator", "test", [])
                 ]) )
        ; ( "md-property-nested-ref"
          , `Quick
          , check_aux "related:: [[programming [[clojure]]]]"
              (Property_Drawer
                 [ ( "related"
                   , "[[programming [[clojure]]]]"
                   , [ I.Nested_link
                         { content = "[[programming [[clojure]]]]"
                         ; children =
                             [ Nested_link.Label "programming "
                             ; Nested_link.Nested_link
                                 ( { content = "[[clojure]]"
                                   ; children = [ Nested_link.Label "clojure" ]
                                   }
                                 , None )
                             ]
                         }
                     ] )
                 ]) )
        ; ( "spaces-before-drawer"
          , `Quick
          , check_aux
              " :PROPERTIES:\n:type: programming_lang\n:creator: test\n:END:"
              (Property_Drawer
                 [ ("type", "programming_lang", []); ("creator", "test", []) ])
          )
        ; ( "endwith-carriage-return"
          , `Quick
          , check_aux
              ":PROPERTIES:\r\n\
               :now: 1614485729874\n\
               :done: 1614485743195\r\n\
               :END:\n"
              (Property_Drawer
                 [ ("now", "1614485729874", []); ("done", "1614485743195", []) ])
          )
        ; ( "endwith-carriage-return-2"
          , `Quick
          , check_aux
              ":PROPERTIES:\r\n\
               :now: 1614485729874\r\n\
               :done: 1614485743195\r\n\
               :END:\r\n"
              (Property_Drawer
                 [ ("now", "1614485729874", []); ("done", "1614485743195", []) ])
          )
        ; ( "simplified-property-syntax"
          , `Quick
          , check_aux "a.b.c:: def\na-b-c::"
              (Property_Drawer [ ("a.b.c", "def", []); ("a-b-c", "", []) ]) )
        ; ( "empty-property"
          , `Quick
          , check_aux ":PROPERTIES:\r\n:END:\r\n" (Property_Drawer []) )
        ; ( "no drawer in quote"
          , `Quick
          , check_aux "> a:: b" (Quote [ plain "a:: b" ]) )
        ] )
  ; ( "inline-code"
    , testcases
        [ ("normal", `Quick, check_aux "`codes here`" (plain "`codes here`"))
        ; ("overlap-with-emphasis", `Quick, check_aux "*aa`*`" (plain "*aa`*`"))
        ; ( "overlap-with-emphasis-2"
          , `Quick
          , check_aux "**aa`**`" (plain "**aa`**`") )
        ; ("overlap-with-emphasis-3", `Quick, check_aux "_a`_`" (plain "_a`_`"))
        ; ( "overlap-with-emphasis-4"
          , `Quick
          , check_aux "__a`__`" (plain "__a`__`") )
        ; ( "overlap-with-emphasis-5"
          , `Quick
          , check_aux "`as*d`*" (plain "`as*d`*") )
        ; ( "overlap-with-link"
          , `Quick
          , check_aux "[as`d](`http://dwdw)" (plain "[as`d](`http://dwdw)") )
        ; ( "overlap-with-link-2"
          , `Quick
          , check_aux "[as`d](http://dwdw)`" (plain "[as`d](http://dwdw)`") )
        ] )
  ; ( "emphasis"
    , testcases
        [ ("normal", `Quick, check_aux "*abc*" (plain "*abc*"))
        ; ("normal-2", `Quick, check_aux "**abc**" (plain "**abc**"))
        ; ("normal-3", `Quick, check_aux "_a_," (plain "_a_,"))
        ; ( "inline-code-inside"
          , `Quick
          , check_aux "*asd`qwe`*" (plain "*asd`qwe`*") )
        ; ( "inline-code-inside-2"
          , `Quick
          , check_aux "***asd`qwe`***" (plain "***asd`qwe`***") )
        ; ("not emphasis (1)", `Quick, check_aux "a * b*" (plain "a * b*"))
        ; ("not emphasis (2)", `Quick, check_aux "a_b_c" (plain "a_b_c"))
        ; ("contains underline", `Quick, check_aux "_a _ a_" (plain "_a _ a_"))
        ; ("contains star", `Quick, check_aux "*a * a*" (plain "*a * a*"))
        ; ( "left flanking delimiter"
          , `Quick
          , check_aux "hello_world_" (plain "hello_world_") )
        ; ( "left flanking delimiter (2)"
          , `Quick
          , check_aux "hello,_world_" (plain "hello,_world_") )
        ; ( "highlight (1)"
          , `Quick
          , check_aux "111==text==222" (plain "111==text==222") )
        ; ( "highlight (2)"
          , `Quick
          , check_aux "111== text==222" (plain "111== text==222") )
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
                 [ I.Plain "[^1]"
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
        [ ("emphasis(1)", `Quick, check_aux "*a\\*b*" (plain "*a*b*"))
        ; ("emphasis(2)", `Quick, check_aux "*a\\\\\\*b*" (plain "*a\\*b*"))
        ; ("code", `Quick, check_aux "`a\\``" (plain "`a``"))
        ; ("nested emphasis", `Quick, check_aux "_a*b\\*_" (plain "_a*b*_"))
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
                 [ Inline.Timestamp
                     (Inline.Scheduled
                        { Timestamp.date = { year = 2004; month = 12; day = 25 }
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
                 [ Inline.Timestamp
                     (Inline.Scheduled
                        { Timestamp.date = { year = 2004; month = 12; day = 25 }
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
                 [ Inline.Timestamp
                     (Inline.Scheduled
                        { Timestamp.date = { year = 2004; month = 12; day = 25 }
                        ; wday = "Sat"
                        ; time = None
                        ; repetition = Some (Timestamp.Plus, Timestamp.Month, 1)
                        ; active = true
                        })
                 ]) )
        ; ( "scheduled after some text"
          , `Quick
          , check_aux "blabla SCHEDULED: <2004-12-25 Sat>"
              (plain "blabla SCHEDULED: <2004-12-25 Sat>") )
        ; ( "deadline"
          , `Quick
          , check_aux "DEADLINE: <2004-12-25 Sat>"
              (paragraph
                 [ Inline.Timestamp
                     (Inline.Deadline
                        { Timestamp.date = { year = 2004; month = 12; day = 25 }
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
                 [ Inline.Timestamp
                     (Inline.Deadline
                        { Timestamp.date = { year = 2004; month = 12; day = 25 }
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
                 [ Inline.Timestamp
                     (Inline.Deadline
                        { Timestamp.date = { year = 2004; month = 12; day = 25 }
                        ; wday = "Sat"
                        ; time = None
                        ; repetition = Some (Timestamp.Plus, Timestamp.Month, 1)
                        ; active = true
                        })
                 ]) )
        ; ( "deadline after some text"
          , `Quick
          , check_aux "blabla DEADLINE: <2004-12-25 Sat>"
              (plain "blabla DEADLINE: <2004-12-25 Sat>") )
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
          , check_aux ">foo\n>bar" (Quote [ plain "foo\nbar" ]) )
        ] )
  ; ( "latex_env"
    , testcases
        [ ( "one-line"
          , `Quick
          , check_aux "\\begin{equation}[a,b,c] x=\\sqrt{b} \\end{equation}"
              (plain "\\begin{equation}[a,b,c] x=\\sqrt{b} \\end{equation}") )
        ] )
  ; ( "list"
    , testcases
        [ ( "heading in list"
          , `Quick
          , check_aux "+ line1\n  - heading"
              (List
                 [ { content = [ plain "line1" ]
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
                 [ { content = [ plain "line1" ]
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
        ; ( "TODO keeps plain title"
          , `Quick
          , check_aux "- TODO todo item"
              (Type.Heading
                 { Type.title =
                     Type_op.inline_list_with_none_pos
                       [ Inline.Plain "todo item" ]
                 ; tags = []
                 ; marker = Some "TODO"
                 ; level = 1
                 ; numbering = None
                 ; priority = None
                 ; anchor = ""
                 ; meta = { Type.timestamps = []; properties = [] }
                 ; unordered = true
                 ; size = None
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
                  { title =
                      Type_op.inline_list_with_none_pos [ Inline.Plain "test" ]
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
                      (I.Scheduled
                         { Timestamp.date =
                             { year = 2004; month = 12; day = 25 }
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
                  { title =
                      Type_op.inline_list_with_none_pos [ Inline.Plain "test" ]
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
                      (I.Scheduled
                         { Timestamp.date =
                             { year = 2004; month = 12; day = 25 }
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
                  { title =
                      Type_op.inline_list_with_none_pos [ Inline.Plain "test" ]
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
                      (I.Scheduled
                         { Timestamp.date =
                             { year = 2004; month = 12; day = 25 }
                         ; wday = "Sat"
                         ; time = None
                         ; repetition = None
                         ; active = true
                         })
                  ]
              ; paragraph
                  [ I.Plain "some "
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
                  { title =
                      Type_op.inline_list_with_none_pos [ Inline.Plain "test" ]
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
                      (I.Scheduled
                         { Timestamp.date =
                             { year = 2004; month = 12; day = 25 }
                         ; wday = "Sat"
                         ; time = None
                         ; repetition = None
                         ; active = true
                         })
                  ]
              ; paragraph
                  [ I.Timestamp
                      (I.Deadline
                         { Timestamp.date =
                             { year = 2004; month = 12; day = 25 }
                         ; wday = "Sat"
                         ; time = None
                         ; repetition = None
                         ; active = true
                         })
                  ]
              ; paragraph
                  [ I.Plain "some "
                  ; I.Link
                      { url = I.Page_ref "page"
                      ; label = [ I.Plain "" ]
                      ; title = None
                      ; full_text = "[[page]]"
                      ; metadata = ""
                      }
                  ]
              ] )
        ; ( "nested headings keep titles"
          , `Quick
          , check_aux2 "- a\n  - b\n    - c"
              [ Type.Heading
                  { title =
                      Type_op.inline_list_with_none_pos [ Inline.Plain "a" ]
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
              ; Type.Heading
                  { title =
                      Type_op.inline_list_with_none_pos [ Inline.Plain "b" ]
                  ; tags = []
                  ; marker = None
                  ; level = 3
                  ; numbering = None
                  ; priority = None
                  ; anchor = ""
                  ; meta = { Type.timestamps = []; properties = [] }
                  ; unordered = true
                  ; size = None
                  }
              ; Type.Heading
                  { title =
                      Type_op.inline_list_with_none_pos [ Inline.Plain "c" ]
                  ; tags = []
                  ; marker = None
                  ; level = 5
                  ; numbering = None
                  ; priority = None
                  ; anchor = ""
                  ; meta = { Type.timestamps = []; properties = [] }
                  ; unordered = true
                  ; size = None
                  }
              ] )
        ; ( "quote with email"
          , `Quick
          , check_aux2 "- > \"CachyOS <admin@cachyos.org>\""
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
              ; Type.Quote [ plain "\"CachyOS <admin@cachyos.org>\"" ]
              ] )
        ; ( "front matter first block"
          , `Quick
          , check_aux2 "---\ntitle: Hello\n---\n- keep title [[page]]"
              [ Type.Directive ("title", "Hello")
              ; Type.Heading
                  { title =
                      Type_op.inline_list_with_none_pos
                        [ Inline.Plain "keep title "
                        ; Inline.Link
                            { url = Inline.Page_ref "page"
                            ; label = [ Inline.Plain "" ]
                            ; title = None
                            ; full_text = "[[page]]"
                            ; metadata = ""
                            }
                        ]
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
        ; ( "front matter only at first block"
          , `Quick
          , check_aux2 "- hello\n---\ntitle: no\n---"
              [ Type.Heading
                  { title =
                      Type_op.inline_list_with_none_pos [ Inline.Plain "hello" ]
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
              ; plain "---\ntitle: no\n---"
              ] )
        ] )
  ]

let () = Alcotest.run "mldoc" @@ List.concat [ inline; block ]

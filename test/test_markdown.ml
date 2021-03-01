let default_config : Conf.t =
  { toc = true
  ; heading_number = true
  ; keep_line_break = false
  ; format = Conf.Markdown
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
        ] )
  ]

let () = Alcotest.run "mldoc" inline

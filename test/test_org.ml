let default_config : Conf.t =
  { toc = true
  ; heading_number = true
  ; keep_line_break = false
  ; format = Conf.Org
  ; heading_to_list = false
  }

let check_mldoc_type =
  Alcotest.check (Alcotest.testable Type.pp ( = )) "check mldoc type"

let check_aux source expect =
  let result = Mldoc.Parser.parse default_config source |> List.hd |> fst in
  fun _ -> check_mldoc_type expect result

let testcases =
  List.map (fun (case, level, f) -> Alcotest.test_case case level f)

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

let () = Alcotest.run "mldoc" @@ List.concat [ block ]

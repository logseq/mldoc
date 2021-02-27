let default_config: Conf.t =
  {toc=true;heading_number=true;keep_line_break=false;format=Conf.Markdown}

let check_mldoc_type = Alcotest.check (Alcotest.testable Type.pp (=)) "check mldoc type"

let check_aux source expect =
  let result = Mldoc.Parser.parse default_config source |> List.hd |> fst in
  check_mldoc_type result expect

let testcases = List.map (fun (case, level, f) -> Alcotest.test_case case level f)

let inline =
  let open Type in
  let module I = Inline in
  [
    ("inline-link", testcases
       [
         "link-inline-normal", `Quick, (fun _ -> check_aux "http://testtest/asdasd"
                                             (Paragraph [I.Link
                                                           {url=I.Complex
                                                                {protocol="http";
                                                                 link="//testtest/asdasd"};
                                                            label= [Plain "http://testtest/asdasd"];
                                                            title= None;
                                                            full_text= "http://testtest/asdasd";
                                                            metadata= ""}]));
         "link-inline-*", `Quick, (fun _ -> check_aux "http://testtest/asd*asd"
                                             (Paragraph [I.Link
                                                           {url=I.Complex
                                                                {protocol="http";
                                                                 link="//testtest/asd*asd"};
                                                            label= [Plain "http://testtest/asd*asd"];
                                                            title= None;
                                                            full_text= "http://testtest/asd*asd";
                                                            metadata= ""}]));
         "spaces-around-link-line", `Quick, (fun _ -> check_aux " http://testtest/asdasd "
                                               (Paragraph [I.Plain " ";
                                                           I.Link
                                                             {url=I.Complex
                                                                  {protocol="http";
                                                                   link="//testtest/asdasd"};
                                                              label= [Plain "http://testtest/asdasd"];
                                                              title= None;
                                                              full_text= "http://testtest/asdasd";
                                                              metadata= ""};
                                                           I.Plain " "]))
       ]);
    ("inline-code", testcases
       [
         "inline-code-normal", `Quick, (fun _ -> check_aux "`codes here`"
                                           (Paragraph [I.Code "codes here"]))
       ]);
    ("inline-macro", testcases
       [
         "inline-macro-double", `Quick, (fun _ -> check_aux "{{test foo}}"
                                            (Paragraph [I.Macro {I.Macro.name = "test"; arguments = ["foo"]}]));
         "inline-macro-three", `Quick, (fun _ -> check_aux "{{{test foo}}}"
                                           (Paragraph [I.Macro {I.Macro.name = "test"; arguments = ["foo"]}]));
         "inline-embed", `Quick, (fun _ -> check_aux "{{{embed [[page]]}}}"
                                     (Paragraph [I.Macro {I.Macro.name = "embed"; arguments = ["[[page]]"]}]))
       ]);
    ("drawer", testcases
       [
         "empty-property-value", `Quick, (fun _ -> check_aux ":PROPERTIES:\n:type: programming_lang\n:creator: test\n:END:"
                                             (Property_Drawer [("type", "programming_lang"); ("creator", "test")]))
       ]);
]

let () =
  Alcotest.run "mldoc" inline

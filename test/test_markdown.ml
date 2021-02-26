
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
                                                            metadata= ""}]))
       ]);
    ("inline-code", testcases
       [
         "inline-code-normal", `Quick, (fun _ -> check_aux "`codes here`"
                                           (Paragraph [I.Code "codes here"]))
       ])
]

let () =
  Alcotest.run "mldoc" inline

open Prelude
module Z = Zip

module String_Tree_Value : sig
  type t = string Z.l

  val of_value : Tree_type.value -> config:Conf.t -> t

  val to_value : t -> Tree_type.value_with_content
end = struct
  type t = string Z.l

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

  let rec of_value v ~config =
    match v with
    | Z.Leaf (t, _pos) ->
      Z.leaf @@ Output.to_string
      @@ Markdown.block (Markdown.default_state ()) config t
    | Z.Branch [] -> Z.Branch []
    | Z.Branch l -> Z.branch @@ List.map (of_value ~config) l

  let collect_body_part l =
    let rec aux r l =
      match l with
      | [] -> (r, [])
      | (Z.Leaf _ as h) :: t -> aux (h :: r) t
      | Z.Branch _ :: _ as rest -> (List.rev r, rest)
    in
    aux [] l

  let to_value t =
    let rec aux t ~level =
      match t with
      | Z.Branch [] -> Z.Branch []
      | Z.Branch [ (Z.Branch _ as e) ] -> aux e ~level
      | Z.Branch (Z.Branch _ :: _ as l) -> Z.branch @@ List.map (aux ~level) l
      | Z.Branch (Z.Leaf h :: t) ->
        (* FIXME: feel bad to operations(concat "-") on string directly here  *)
        let h = "- " ^ h in
        let ast = Mldoc_parser.parse default_config h in
        let body, rest = collect_body_part t in
        let body_ast =
          List.map
            (function
              | Z.Leaf e ->
                let ast = Mldoc_parser.parse default_config e in
                let ast' =
                  List.map
                    (fun (ast, (pos : Pos.pos_meta)) ->
                      ( ast
                      , String.sub e pos.start_pos (pos.end_pos - pos.start_pos)
                      ))
                    ast
                in
                Some ast'
              | Z.Branch _ -> None)
            body
          |> List.concat_map (function
               | None -> []
               | Some e -> e)
        in
        let head_ast, body_ast' =
          match ast with
          | [] -> failwith "empty ast"
          | h :: t -> (h, t)
        in
        let head, pos = head_ast in
        let head' =
          match head with
          | Type.Heading h -> Type.Heading { h with level }
          | _ -> head
        in
        let head_content =
          String.sub h (2 + pos.start_pos) (pos.end_pos - 2 - pos.start_pos)
        in
        let head_ast' = (head', head_content) in
        let body_ast' =
          List.map
            (fun (ast, (pos : Pos.pos_meta)) ->
              (ast, String.sub h pos.start_pos (pos.end_pos - pos.start_pos)))
            body_ast'
        in
        let body_ast'' = body_ast' @ body_ast in
        Z.branch
        @@ (Z.leaf head_ast' :: List.map Z.leaf body_ast'')
        @ List.map (aux ~level:(level + 1)) rest
      | Z.Leaf _ as l -> aux (Z.branch [ l ]) ~level
    in
    aux t ~level:1
end

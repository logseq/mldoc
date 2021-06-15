open! Prelude
open Xmlm

type headers = { title : string } [@@deriving yojson]

type headers_and_blocks = headers * Type.blocks_with_content [@@deriving yojson]

let parse_header (i : input) =
  let headers : headers = { title = "untitled" } in
  let rec aux headers i =
    match peek i with
    | `El_start ((_, "body"), _) -> headers
    | `El_start ((_, "title"), _) -> (
      input i |> ignore;
      match input i with
      | `Data title -> aux { title = String.trim title } i
      | _ -> aux headers i)
    | `El_start _
    | `El_end
    | `Data _
    | `Dtd _ ->
      input i |> ignore;
      aux headers i
  in
  aux headers i

type tree =
  | E of Xmlm.tag * tree list
  | D

let body_to_tree i =
  Xmlm.input_tree ~el:(fun tag childs -> E (tag, childs)) ~data:(fun _ -> D) i

let tree_to_string_tree_type tree =
  let open Option in
  let extracted_body =
    match tree with
    | E (((_, "body"), _), childs) -> childs
    | _ -> []
  in
  let rec aux (tree : tree) =
    match tree with
    | E (((_, "outline"), attrs), childs) ->
      let text =
        List.find_map
          (function
            | (_, "text"), text -> Some text
            | _ -> None)
          attrs
        |? ""
      in
      let note =
        List.find_map
          (function
            | (_, "_note"), note -> Some note
            | _ -> None)
          attrs
      in
      let text_and_note =
        match note with
        | None -> [ Zip.Leaf text ]
        | Some note' -> [ Zip.Leaf text; Zip.Leaf note' ]
      in
      Zip.branch (List.append text_and_note (List.map aux childs))
    | E _
    | D ->
      Zip.branch []
  in
  let remove_empty_branch l =
    let open Zip in
    let z = of_l l in
    let rec aux z =
      if is_end z then
        root z
      else
        match node z with
        | Branch [] -> (
          match remove z with
          | Some z' -> aux (next z')
          | None -> aux (next z))
        | _ -> aux (next z)
    in
    aux z
  in
  remove_empty_branch @@ Zip.branch @@ List.map aux extracted_body

let string_tree_type_to_value = Markdown_transformer.String_Tree_Value.to_value

let parse opml_string =
  let input = make_input (`String (0, opml_string)) in
  let headers = parse_header input in
  let blocks =
    body_to_tree input |> tree_to_string_tree_type
    |> Markdown_transformer.String_Tree_Value.to_value
    |> Tree_type.of_value_with_content |> Tree_type.to_blocks_with_content
  in
  (headers, blocks)

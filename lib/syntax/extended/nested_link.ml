(* Syntax from https://roamresearch.com *)

(* [[Introduction to [[Logseq]]]] *)

open! Prelude
open Angstrom
open Parsers

type child =
  | Label of string
  | Nested_link of t_with_pos

and children = child list

and t =
  { content : string
  ; children : children
  }

and t_with_pos = t * Pos.pos_meta option

let rec to_yojson { content; children } : Yojson.Safe.t =
  let child_to_yojson child =
    match child with
    | Label s -> `List [ `String "Label"; `String s ]
    | Nested_link (t, None) -> `List [ `String "Nested_link"; to_yojson t ]
    | Nested_link (t, Some pos) ->
      `List
        [ `String "Nested_link"
        ; `List [ to_yojson t; Pos.pos_meta_to_yojson pos ]
        ]
  in
  `Assoc
    [ ("content", `String content)
    ; ("children", `List (List.map child_to_yojson children))
    ]

let rec child_of_yojson (json : Yojson.Safe.t) =
  let ( >>= ) = Ppx_deriving_yojson_runtime.( >>= ) in
  match json with
  | `List [ `String "Label"; `String s ] -> Ok (Label s)
  | `List [ `String "Nested_link"; (`Assoc _ as t) ] ->
    of_yojson t >>= fun t' -> Ok (Nested_link (t', None))
  | `List [ `String "Nested_link"; `List [ t; pos ] ] ->
    Pos.pos_meta_of_yojson pos >>= fun pos' ->
    of_yojson t >>= fun t' -> Ok (Nested_link (t', Some pos'))
  | _ -> Result.Error "invalid_arg: child_of_yojson"

and of_yojson (json : Yojson.Safe.t) =
  match json with
  | `Assoc [ ("content", `String content); ("children", `List children) ] ->
    let children' =
      Prelude.List.map
        (fun child ->
          match child_of_yojson child with
          | Ok v -> [ v ]
          | _ -> [])
        children
      |> Prelude.List.flatten
    in
    Ok { content; children = children' }
  | _ -> Result.Error "invalid_arg: Nested_link.of_yojson"

let rec forward_pos t forward =
  let open Pos in
  let { children; _ } = t in
  { t with
    children =
      List.map
        (fun child ->
          match child with
          | Nested_link (t, Some pos) ->
            Nested_link
              ( forward_pos t forward
              , Some
                  { start_pos = pos.start_pos + forward
                  ; end_pos = pos.end_pos + forward
                  } )
          | _ -> child)
        children
  }

let open_brackets = "[["

let close_brackets = "]]"

let match_brackets () =
  let level_ref = ref 1 in
  let s_ref = ref "" in
  (* To determine whether the `]` is inside a string. *)
  string "[["
  *> fix (fun parse ->
         end_string_2 close_brackets ~ci:true (fun s ->
             let level = count_substring s open_brackets in
             let _ = level_ref := !level_ref + level - 1 in
             let _ = s_ref := !s_ref ^ s ^ close_brackets in
             if !level_ref <= 0 then
               let v = !s_ref in
               let _ = level_ref := 1 in
               let _ = s_ref := "" in
               return (open_brackets ^ v)
             else
               parse))

let label_parse = take_while1 (fun c -> c <> '[') >>| fun s -> Label s

let parse (config : Conf.t) =
  let open Pos in
  let prefix_pos = Stack.create () in
  Stack.push 0 prefix_pos;
  fix (fun p ->
      let children_parse = many1 (choice [ label_parse; p ]) in
      pos >>= fun start_pos ->
      let start_pos = start_pos + Stack.top prefix_pos in
      match_brackets () >>= fun s ->
      pos >>= fun end_pos ->
      let end_pos = end_pos + Stack.top prefix_pos in
      Stack.push (start_pos + 2) prefix_pos;
      let inner_s = String.sub s 2 (String.length s - 4) in
      let result =
        match parse_string ~consume:All children_parse inner_s with
        | Ok result -> result
        | Error _e -> [ Label inner_s ]
      in
      let _ = Stack.pop prefix_pos in
      let pos =
        if config.inline_type_with_pos then
          Some { start_pos; end_pos }
        else
          None
      in
      return @@ Nested_link ({ content = s; children = result }, pos))
  >>= function
  | Label _l -> fail "nested link"
  | Nested_link ({ content; children }, _) ->
    if List.length children <= 1 then
      fail "nested link"
    else
      return @@ { content; children }

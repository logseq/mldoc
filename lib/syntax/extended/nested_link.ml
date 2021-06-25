(* Syntax from https://roamresearch.com *)

(* [[Introduction to [[Logseq]]]] *)

open Prelude
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
[@@deriving yojson]

and t_with_pos = t * Pos.pos_meta option

let t_with_pos_to_yojson t_with_pos : Yojson.Safe.t =
  let t, pos = t_with_pos in
  let t_yojson = to_yojson t in
  match pos with
  | None -> t_yojson
  | Some pos' ->
    let pos_yojson = Pos.pos_meta_to_yojson pos' in
    `List [ t_yojson; pos_yojson ]

let t_with_pos_of_yojson (json : Yojson.Safe.t) =
  let open Ppx_deriving_yojson_runtime in
  match json with
  | `List [ t; pos ] ->
    of_yojson t >>= fun t' ->
    Pos.pos_meta_of_yojson pos >|= fun pos' -> (t', Some pos')
  | `Assoc _ as t -> of_yojson t >|= fun t' -> (t', None)
  | _ -> Result.Error "invalid_arg: Nested_link.t_with_pos_of_yojson"

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
  | Nested_link ({ content; children }, pos) ->
    if List.length children <= 1 then
      fail "nested link"
    else
      return @@ ({ content; children }, pos)

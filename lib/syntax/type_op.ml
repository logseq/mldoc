open! Prelude
open Pos
open Type

let remove_properties =
  List.filter (function
    | Type.Property_Drawer _ -> false
    | _ -> true)

let inline_list_with_dummy_pos (l : 'a list) : ('a * pos_meta option) list =
  List.map (fun i -> (i, Some Pos.dummy_pos)) l

let inline_list_with_none_pos (l : 'a list) : ('a * pos_meta option) list =
  List.map (fun i -> (i, None)) l

let inline_list_strip_pos (l : ('a * pos_meta option) list) : 'a list =
  List.map fst l

let inline_with_pos i start_pos end_pos =
  (i, Some ({ start_pos; end_pos } : Pos.pos_meta))

let inline_with_dummy_pos i = (i, Some Pos.dummy_pos)

let inline_move_forward (i, pos) ~forward_pos =
  ( i
  , match pos with
    | None -> None
    | Some { start_pos; end_pos } ->
      Some
        { start_pos = start_pos + forward_pos; end_pos = end_pos + forward_pos }
  )

let inline_list_move_forward l forward_pos =
  List.map (inline_move_forward ~forward_pos) l

let rec type_move_forawrd t forward_pos =
  match t with
  | Paragraph l -> Paragraph (inline_list_move_forward l forward_pos)
  | Heading h ->
    Heading { h with title = inline_list_move_forward h.title forward_pos }
  | List items ->
    List
      (List.map
         (fun l ->
           { l with
             content =
               List.map (fun t -> type_move_forawrd t forward_pos) l.content
           ; name = inline_list_move_forward l.name forward_pos
           })
         items)
  | Quote l -> Quote (List.map (fun t -> type_move_forawrd t forward_pos) l)
  | Footnote_Definition (s, l) ->
    Footnote_Definition (s, inline_list_move_forward l forward_pos)
  | _ -> t

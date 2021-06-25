open! Prelude
open Pos

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

let inline_list_move_forward l forward_pos =
  List.map
    (fun (i, pos) ->
      ( i
      , match pos with
        | None -> None
        | Some { start_pos; end_pos } ->
          Some
            { start_pos = start_pos + forward_pos
            ; end_pos = end_pos + forward_pos
            } ))
    l

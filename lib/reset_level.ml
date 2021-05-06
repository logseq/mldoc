type state =
  { indent : [ `Must of int | `Maybe of int ] option
  ; indent_threshold : int
  }

let empty_state = { indent = None; indent_threshold = 0 }

let update_level (state, r) (t, pos) =
  match t with
  | Type.Heading h -> (
    match (h.unordered, state.indent) with
    | true, Some (`Maybe indent') when h.level <= indent' ->
      ( { indent = Some (`Must indent'); indent_threshold = h.level }
      , (Type.Heading { h with level = h.level + indent' }, pos) :: r )
    | true, Some (`Maybe _) -> (empty_state, (t, pos) :: r)
    | true, Some (`Must indent') when h.level >= state.indent_threshold ->
      (state, (Type.Heading { h with level = h.level + indent' }, pos) :: r)
    | true, Some (`Must _indent') -> (empty_state, (t, pos) :: r)
    | true, None -> (empty_state, (t, pos) :: r)
    | false, _ ->
      ( { indent = Some (`Maybe 4); indent_threshold = 0 }
      , (Type.Heading { h with level = 1 }, pos) :: r ))
  | _ -> (state, (t, pos) :: r)

(* reset Heading's level
   ---
   ## AA (reset level=1)
   - BB (reset level=1+4)
       - CC (reset level=5+4)
   #### DD (reset level=1)
       - EE (level=5, unchanged)
   - FF (level=1, unchanged)
   ---
   *)
let reset (tl : (Type.t * Type.pos_meta) list) =
  List.fold_left update_level (empty_state, []) tl |> snd |> List.rev

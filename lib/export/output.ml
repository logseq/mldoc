open Prelude

type t =
  | RawText of string
  | Space  (** merge adjacent multiple Space as one Space *)
  | Newline  (** merge adjacent multiple Newline as one Newline *)
  | TwoNewlines
  | OneNewline  (** merge_adjacent_space_newline internal use *)
  | Indent of (int * int)  (** indent (num of tabs, num of spaces after tabs) *)

(* 1.  [...;space; space]         -> [...;space]
   2.  [...;newline;newline]      -> [...;newline]
   3.  [...;space; newline]       -> [...;newline]
   4.  [...;newline;space]        -> [...;newline]
   5.  [...;"XXX\n";space]        -> [...;"XXX\n"]
   6.  [...;space;"\nXXX"]        -> [...;"\nXXX"]
   7.  [...;"XXX\n";newline]      -> [...;"XXX\n"]
   8.  [...;newline;"\nXXX"]      -> [...;"\nXXX"]
   9.  [...;"XXX<space>";space]   -> [...;"XXX<space>"]
   10. [...;space;"<space>XXX"]   -> [...;"<space>XXX"]
   11. [...;"XXX<space>";newline] -> [...;"XXX<space>";newline]
   12. [...;space;"XXX"]          -> [...;space;"XXX"]
   13. [...;newline;"XXX"]        -> [...;newline;"XXX"]
   14. [...;space;indent]         -> [...;indent]
   15. [...;indent;indent]        -> [...;indent]
   16. [...;newline;indent]       -> [...;newline;indent]
   17. [...;indent;space]         -> [...;indent]
   18. [...;indent;newline]       -> [...;newline]
   19. [...;"XXX\n";indent]       -> [...;"XXX\n";indent]
   20. [...;"XXX<space>";indent]  -> [...;"XXX<space>"]
   21. [...;indent;"XXX"]         -> [...;indent;"XXX"]
   22. [...;indent;"\nXXX"]       -> [...;"\nXXX"]
   23. [...;"XXX";indent]         -> [...;"XXX"] // XXX is not endwith '\n'
 * *)
let merge_adjacent_space_newline =
  let suffix_newline_num s =
    let len = String.length s in
    if len = 0 then
      None
    else if len = 1 && s.[0] = '\n' then
      Some 1
    else if len > 1 && s.[len - 1] = '\n' && s.[len - 2] = '\n' then
      Some 2
    else if len > 1 && s.[len - 1] = '\n' then
      Some 1
    else
      None
  in
  let to_t = function
    | `Space -> Space
    | `Newline -> Newline
    | `TwoNewlines -> TwoNewlines
    | `OneNewline -> OneNewline
    | `Indent n -> Indent n
  in
  fun tl ->
    List.rev
    @@ (fun (r, _, _, _, _) -> r)
    @@ List.fold_left
         (fun ( result
              , before
              , before_space_text
              , before_newline_text
              , start_of_line ) e ->
           match
             (e, before, before_space_text, before_newline_text, start_of_line)
           with
           | Space, _, _, Some _, _ ->
             (* 5 *) (result, [], false, before_newline_text, start_of_line)
           | Space, _, true, None, _ ->
             (* 9 *) (result, [], true, None, start_of_line)
           | Space, `Space :: _, false, None, _ ->
             (* 1 *)
             (result, [ `Space ], false, None, start_of_line)
           | Space, `Newline :: _, false, None, _ ->
             (* 4 *)
             (result, [ `Newline ], false, None, start_of_line)
           | Space, `Indent n :: _, false, None, _ ->
             (* 17 *)
             (result, [ `Indent n ], false, None, start_of_line)
           | Space, `OneNewline :: _, false, None, _ ->
             (result, [ `OneNewline ], false, None, start_of_line)
           | Space, `TwoNewlines :: _, false, None, _ ->
             (result, [ `TwoNewlines ], false, None, start_of_line)
           | Space, [], false, None, _ ->
             (* 12 *)
             (result, [ `Space ], false, None, start_of_line)
           | Newline, _, _, Some _, _ ->
             (*  7 *) (result, [], false, before_newline_text, true)
           | Newline, _, true, None, _ ->
             (* 11 *)
             (result, [ `Newline ], false, None, true)
           | Newline, `Space :: _, false, None, _ ->
             (* 3 *)
             (result, [ `Newline ], false, None, true)
           | Newline, `Newline :: _, false, None, _ ->
             (* 2 *)
             (result, [ `Newline ], false, None, true)
           | Newline, `Indent _ :: _, false, None, _ ->
             (* 18 *)
             (result, [ `Newline ], false, None, true)
           | Newline, `OneNewline :: _, false, None, _ ->
             (result, [ `OneNewline ], false, None, true)
           | Newline, `TwoNewlines :: _, false, None, _ ->
             (result, [ `TwoNewlines ], false, None, true)
           | Newline, [], false, None, _ ->
             (result, [ `Newline ], false, None, true)
           | OneNewline, _, _, Some _, _ ->
             (* before_newline_text can't erase OneNewline *)
             (result, [ `OneNewline ], false, None, true)
           | OneNewline, _, true, None, _ ->
             (result, [ `OneNewline ], false, None, true)
           | OneNewline, `Space :: _, false, None, _ ->
             (result, [ `OneNewline ], false, None, true)
           | OneNewline, `Newline :: _, false, None, _ ->
             (result, [ `OneNewline ], false, None, true)
           | OneNewline, `Indent _ :: _, false, None, _ ->
             (result, [ `OneNewline ], false, None, true)
           | OneNewline, `TwoNewlines :: _, false, None, _ ->
             (result, [ `TwoNewlines ], false, None, true)
           | OneNewline, `OneNewline :: _, false, None, _ ->
             (result, [ `OneNewline ], false, None, true)
           | OneNewline, [], false, None, _ ->
             (result, [ `OneNewline ], false, None, true)
           | TwoNewlines, _, _, Some 2, _ ->
             (result, [], false, before_newline_text, true)
           | TwoNewlines, _, _, Some _, _ ->
             (result, [ `OneNewline ], false, before_newline_text, true)
           | TwoNewlines, _, true, None, _ ->
             (result, [ `TwoNewlines ], false, None, true)
           | TwoNewlines, `Space :: _, false, None, _ ->
             (result, [ `TwoNewlines ], false, None, true)
           | TwoNewlines, `Newline :: _, false, None, _ ->
             (result, [ `TwoNewlines ], false, None, true)
           | TwoNewlines, `Indent _ :: _, false, None, _ ->
             (result, [ `TwoNewlines ], false, None, true)
           | TwoNewlines, `OneNewline :: _, false, None, _ ->
             (result, [ `TwoNewlines ], false, None, true)
           | TwoNewlines, `TwoNewlines :: _, false, None, _ ->
             (result, [ `TwoNewlines ], false, None, true)
           | TwoNewlines, [], false, None, _ ->
             (result, [ `TwoNewlines ], false, None, true)
           | Indent (n, _), a, b, c, _ when n <= 0 ->
             (* ignore *)
             (result, a, b, c, start_of_line)
           | Indent n, before, _, _, true ->
             ( CCList.append (CCList.map to_t before) result
             , [ `Indent n ]
             , false
             , None
             , false )
           | Indent n, _, _, Some _, _ ->
             (* 19 *)
             (result, [ `Indent n ], false, None, start_of_line)
           | Indent _, _, true, None, false ->
             (* 20 *)
             (result, before, true, None, start_of_line)
           | Indent _, `Space :: _, false, None, false ->
             (* 14 *)
             (result, [ `Space ], false, None, start_of_line)
           | Indent n, `Indent _ :: _, false, None, false ->
             (* 15 *)
             (result, [ `Indent n ], false, None, start_of_line)
           | Indent n, `Newline :: _, false, None, false ->
             (* 16 *)
             (Newline :: result, [ `Indent n ], false, None, true)
           | Indent n, `OneNewline :: _, false, None, false ->
             (OneNewline :: result, [ `Indent n ], false, None, true)
           | Indent n, `TwoNewlines :: _, false, None, false ->
             (TwoNewlines :: result, [ `Indent n ], false, None, true)
           | Indent _, [], false, None, false ->
             (* 23 *)
             (result, [], false, None, false)
           | RawText "", _, _, _, _ ->
             ( result
             , before
             , before_space_text
             , before_newline_text
             , start_of_line )
           | RawText s, `Space :: _, _, _, _ when s.[0] = '\n' || s.[0] = ' ' ->
             (* 6,10 *)
             let last_char = s.[String.length s - 1] in
             ( e :: result
             , []
             , last_char = ' '
             , suffix_newline_num s
             , last_char = '\n' )
           | RawText s, `Space :: _, _, _, _ ->
             (* 12 *)
             let last_char = s.[String.length s - 1] in
             ( e :: Space :: result
             , []
             , last_char = ' '
             , suffix_newline_num s
             , last_char = '\n' )
           | RawText s, `Newline :: _, _, _, _ when s.[0] = '\n' ->
             (* 8 *)
             let last_char = s.[String.length s - 1] in
             ( e :: result
             , []
             , last_char = ' '
             , suffix_newline_num s
             , last_char = '\n' )
           | RawText s, `Newline :: _, _, _, _ ->
             (* 13 *)
             let last_char = s.[String.length s - 1] in
             ( e :: Newline :: result
             , []
             , last_char = ' '
             , suffix_newline_num s
             , last_char = '\n' )
           | RawText s, `OneNewline :: _, _, _, _ when s.[0] = '\n' ->
             let last_char = s.[String.length s - 1] in
             ( e :: result
             , []
             , last_char = ' '
             , suffix_newline_num s
             , last_char = '\n' )
           | RawText s, `OneNewline :: _, _, _, _ ->
             let last_char = s.[String.length s - 1] in
             ( e :: OneNewline :: result
             , []
             , last_char = ' '
             , suffix_newline_num s
             , last_char = '\n' )
           | RawText s, `TwoNewlines :: _, _, _, _ ->
             let result =
               if String.length s = 1 then
                 if s.[0] = '\n' then
                   TwoNewlines :: result
                 else
                   e :: TwoNewlines :: result
               else if s.[0] = '\n' && s.[1] = '\n' then
                 e :: result
               else if s.[0] = '\n' then
                 RawText (String.sub s 1 (String.length s - 1))
                 :: TwoNewlines :: result
               else
                 e :: TwoNewlines :: result
             in
             let last_char = s.[String.length s - 1] in
             ( result
             , []
             , last_char = ' '
             , suffix_newline_num s
             , last_char = '\n' )
           | RawText s, `Indent _ :: _, _, _, _ when s.[0] = '\n' ->
             (* 22 *)
             let last_char = s.[String.length s - 1] in
             ( e :: result
             , []
             , last_char = ' '
             , suffix_newline_num s
             , last_char = '\n' )
           | RawText s, `Indent n :: _, _, _, _ ->
             (* 21 *)
             let last_char = s.[String.length s - 1] in
             ( e :: Indent n :: result
             , []
             , last_char = ' '
             , suffix_newline_num s
             , last_char = '\n' )
           | RawText s, [], _, _, _ ->
             let last_char = s.[String.length s - 1] in
             ( e :: result
             , []
             , last_char = ' '
             , suffix_newline_num s
             , last_char = '\n' ))
         ([], [ `Space ], false, None, true)
         tl

let remove_fst_space_newline = function
  | [] -> []
  | Space :: t -> t
  | Newline :: t -> t
  | l -> l

let to_string tl =
  String.concat ""
  @@ CCList.map
       (function
         | Space -> " "
         | Newline -> "\n"
         | TwoNewlines -> "\n\n"
         | OneNewline -> "\n"
         | Indent (tabs, spaces) ->
           let tabs' =
             if tabs > 0 then
               String.make tabs '\t'
             else
               ""
           in
           let spaces' =
             if spaces > 0 then
               String.make spaces ' '
             else
               ""
           in
           tabs' ^ spaces'
         | RawText s -> s)
       (remove_fst_space_newline
          (merge_adjacent_space_newline (merge_adjacent_space_newline tl)))

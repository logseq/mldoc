open! Prelude
open Angstrom
open Parsers

let inline_choices config : Inline.t_with_pos Angstrom.t =
  let p =
    peek_char_fail >>= function
    | '#' -> Inline.hash_tag config
    | '[' -> Inline.nested_link_or_link config
    | '(' -> Inline.block_reference config
    (* | '{' -> macro config *)
    | 'S'
    | 'C'
    | 'D'
    | 's'
    | 'c'
    | 'd' ->
      Inline.timestamp
    | _ ->
      take_till1 (fun c ->
          c = '#' || c = '[' || c = '('
        )
      >>= fun _ -> return (Inline.Plain "")
  in
  (fun t -> (t, None)) <$> p

let parse config =
  many1 (inline_choices config)
  >>| (fun l -> Inline.concat_plains l)
  <?> "inline"

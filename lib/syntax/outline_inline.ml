open! Prelude
open Angstrom
open Parsers

let empty_plain _ = return (Inline.Plain "")

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
    | '`' -> Inline.code config <|> (any_char >>= empty_plain)
    | ' '
    | '\t'
    | '\n'
    | '\r'
    | '\012' ->
      any_char >>= empty_plain
    | _ ->
      take_till1 (fun c -> c = '#' || c = '[' || c = '(' || c = '`')
      >>= empty_plain
  in
  (fun t -> (t, None)) <$> p

let parse config =
  many1 (inline_choices config)
  >>| (fun l -> Inline.concat_plains l)
  <?> "inline"

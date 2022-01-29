open! Prelude
open Angstrom
open Parsers
open Conf

let empty_plain _ = return (Inline.Plain "")

let skip_char = any_char >>= empty_plain

let in_plain_delims config c =
  match config.format with
  | Markdown -> c = '[' || c = '`' || is_whitespace c
  | Org -> c = '[' || c = '=' || c = '~' || is_whitespace c

let skip_plain config = take_till (in_plain_delims config) >>= empty_plain

let inline_code config = Inline.code config >>= empty_plain

let inline_choices config : Inline.t_with_pos Angstrom.t =
  let skip_plain = any_char *> skip_plain config in
  let p =
    peek_char_fail >>= function
    | '#' -> Inline.hash_tag config
    | '[' -> Inline.nested_link_or_link config
    | '(' -> Inline.block_reference config
    | 'S'
    | 'C'
    | 'D'
    | 's'
    | 'c'
    | 'd' ->
      Inline.timestamp
    | c -> (
      if is_whitespace c then
        skip_char
      else
        match config.format with
        | Markdown ->
          if c = '`' then
            inline_code config
          else
            fail "inline choice"
        | Org ->
          if c = '=' || c = '~' then
            inline_code config
          else
            fail "inline choice")
  in
  let p' = p <|> skip_plain <|> skip_char in
  (fun t -> (t, None)) <$> p'

let parse config =
  many1 (inline_choices config)
  >>| (fun l ->
        let l = remove (fun (t, _) -> t = Inline.Plain "") l in
        Inline.concat_plains l)
  <?> "inline"

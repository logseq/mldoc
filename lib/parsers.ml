open Angstrom
open Util

let is_space = function
  | ' ' | '\010' | '\013' | '\009' | '\026' | '\012' -> true
  | _ -> false

let non_space = not << is_space

let is_eol = function '\r' | '\n' -> true | _ -> false

let non_eol = not << is_eol

let non_space_eol c =
  non_space c && non_eol c

let is_hex = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let digits = take_while1 is_digit

let is_uppercase c = 'A' <= c && c <= 'Z'
let is_lowercase c = 'a' <= c && c <= 'z'
let is_letter c =
  is_uppercase c || is_lowercase c

let eols = take_while1 is_eol

let ws = take_while1 is_space

let spaces = skip_while is_space

let count_spaces = take_while is_space

let lex p = p <* spaces

let optional p = option None (lift (fun x -> Some x) p)

let optional_list p = option [] p

let lift5 f a b c d e = lift4 f a b c d <*> e

let between_char c1 c2 p = char c1 *> p <* char c2

let between_string begin' end' p = string begin' *> p <* string end'

let chainl1 e op =
  let rec go acc =
    (lift2 (fun f x -> f acc x) op e >>= go) <|> return acc in
  e >>= fun init -> go init

let double_chars c1 c2 f =
  let prev = ref None in
  take_while1 (fun c ->
      let p = !prev in
      prev := Some c;
      if (c == c2 && p == Some c1) || is_eol c then false
      else true)
  >>| fun s ->
  let s = String.sub s 0 (String.length s - 2) in
  f s

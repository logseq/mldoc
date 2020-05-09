open Angstrom
open Prelude

let is_space = function
  | ' ' | '\009' | '\026' | '\012' -> true
  | _ -> false

let non_space = not << is_space

(* end of line *)
let is_eol = function '\r' | '\n' -> true | _ -> false

let non_eol = not << is_eol

let non_space_eol c =
  non_space c && non_eol c

let is_hex = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let digits = take_while1 is_digit

let eol = satisfy is_eol
let eols = take_while1 is_eol
let two_eols result = eol *> eol *> return result

let ws = take_while1 is_space

let spaces = skip_while is_space
let spaces_or_eols = skip_while (fun c -> is_eol c || is_space c)

let non_spaces = take_while1 non_space_eol

let letters = take_while1 is_letter

let count_spaces = take_while is_space

let lex p = p <* spaces

let optional p = option None (lift (fun x -> Some x) p)

let optional_list p = option [] p

let lift5 f a b c d e = lift4 f a b c d <*> e

let between_char c1 c2 p = char c1 *> p <* char c2

let between_string begin' end' p = string begin' *> p <* string end'

let between_string_ci begin' end' p = string_ci begin' *> p <* string end'

let chainl1 e op =
  let rec go acc =
    (lift2 (fun f x -> f acc x) op e >>= go) <|> return acc in
  e >>= fun init -> go init

let end_string s ?ci:(ci=false) f =
  let open String in
  let last_s = sub s (length s - 1) 1 in
  let prev = ref None in
  let string_equal x y = if ci then lowercase_ascii x = lowercase_ascii y else x = y in
  take_while1 (fun c ->
      let p = (match !prev with
            None -> (make 1 c)
          | Some s' -> let s' = s' ^ (make 1 c) in
            if length s' > length s then
              sub s' 1 (length s)
            else s'
        ) in
      prev := Some p;
      if (string_equal p s) then false
      else true) <* (string last_s)
  >>= fun s' ->
  let p = !prev in
  prev := None;
  match p with
  | None -> fail "end string"
  | Some x ->
    if string_equal x s then
      let s' = sub s' 0 (length s' - length s + 1) in
      return @@ f s'
    else
      fail "end_string"

let peek_line = take_till (fun c -> c = '\r' || c = '\n')
                |> unsafe_lookahead

let peek_spaces = ws |> unsafe_lookahead

let take_till1 f = take_while1 (fun c -> not (f c))

let line = take_till1 is_eol

let line_without_spaces = take_till1 (fun c -> c = '\r' || c = '\n' || c = ' ')

let word = take_till1 non_space

let clear_parser_resource p r error =
  p r >>= fun result ->
  r := [];
  return result
  <|>
  let _ = r := [] in
  fail error

let between_lines ?trim:(trim=true) end_check error =
  let p lines =
    fix (fun body_parser ->
        optional eols *> take_till1 is_eol <* optional eols >>= fun line ->
        let line = if trim then String.trim line else line in
        if end_check line then
          return (List.rev !lines)
        else
          let _ = lines := line :: !lines in
          body_parser) in
  clear_parser_resource p (ref []) error

let between_eols p =
  optional eols *> optional spaces *> p <* optional eols

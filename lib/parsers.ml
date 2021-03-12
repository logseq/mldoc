open Angstrom
open Prelude

let is_space = function
  | ' '
  | '\009'
  | '\026'
  | '\012' ->
    true
  | _ -> false

let is_tab = function
  | '\t' -> true
  | _ -> false

let is_tab_or_space = function
  | '\t'
  | ' '
  | '\026'
  | '\012' ->
    true
  | _ -> false

let non_tab_or_space = not << is_tab_or_space

let non_space = not << is_space

(* end of line *)
let is_eol = function
  | '\r'
  | '\n' ->
    true
  | _ -> false

let non_eol = not << is_eol

let non_space_eol c = non_space c && non_eol c

let is_hex = function
  | '0' .. '9'
  | 'a' .. 'f'
  | 'A' .. 'F' ->
    true
  | _ -> false

let digits = take_while1 is_digit

let eol = string "\n" <|> string "\r\n"

let eols = take_while1 is_eol

let two_eols result = eol *> eol *> return result

let ws = take_while1 is_space

let tabs = take_while1 is_tab

let tabs_or_ws = take_while1 is_tab_or_space

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
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init

let end_string s ?(ci = false) f =
  let open String in
  let last_s = sub s (length s - 1) 1 in
  let prev = ref None in
  let string_equal x y =
    if ci then
      lowercase_ascii x = lowercase_ascii y
    else
      x = y
  in
  take_while1 (fun c ->
      let p =
        match !prev with
        | None -> make 1 c
        | Some s' ->
          let s' = s' ^ make 1 c in
          if length s' > length s then
            sub s' 1 (length s)
          else
            s'
      in
      prev := Some p;
      if string_equal p s then
        false
      else
        true)
  <* string last_s
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

let end_string_2 s ?(ci = false) f =
  let open String in
  let last_s = sub s (length s - 1) 1 in
  let prev = ref None in
  let string_equal x y =
    if ci then
      lowercase_ascii x = lowercase_ascii y
    else
      x = y
  in
  take_while (fun c ->
      let p =
        match !prev with
        | None -> make 1 c
        | Some s' ->
          let s' = s' ^ make 1 c in
          if length s' > length s then
            sub s' 1 (length s)
          else
            s'
      in
      prev := Some p;
      if string_equal p s then
        false
      else
        true)
  <* string last_s
  >>= fun s' ->
  let p = !prev in
  prev := None;
  match p with
  | None -> fail "end string"
  | Some x ->
    if string_equal x s then
      let s' = sub s' 0 (length s' - length s + 1) in
      f s'
    else
      fail "end_string"

let between_string_strict begin' end' ?(ci = false) f =
  string begin' *> end_string end' ~ci f

let between_string_strict_wrapper ?(ci = false) begin' end' =
  string begin'
  *> end_string end' ~ci (fun s -> String.concat "" [ begin'; s; end' ])

let peek_line = take_till (fun c -> c = '\r' || c = '\n') |> unsafe_lookahead

let peek_spaces = ws |> unsafe_lookahead

let take_till1 f = take_while1 (fun c -> not (f c))

let line = take_till1 is_eol

let optional_line = take_till is_eol

let line_without_spaces = take_till1 (fun c -> c = '\r' || c = '\n' || c = ' ')

let word = take_till1 non_space

let clear_parser_resource p r error =
  p r >>= fun result ->
  r := [];
  return result
  <|>
  let _ = r := [] in
  fail error

let between_lines ?(trim = true) end_check error =
  let p lines =
    fix (fun body_parser ->
        line <|> (eol >>| fun _ -> "\n") >>= fun line ->
        let line =
          if trim then
            String.trim line
          else
            line
        in
        if end_check line then
          let lines = List.rev !lines in
          return lines
        else
          let _ = lines := line :: !lines in
          body_parser)
  in
  clear_parser_resource p (ref []) error

let between_eols p = optional eols *> optional spaces *> p <* optional eols

let rec at_most m p =
  if m = 0 then
    return []
  else
    lift2 (fun x xs -> x :: xs) p (at_most (m - 1) p) <|> return []

let limits n m p = lift2 (fun xs ys -> xs @ ys) (count n p) (at_most m p)

let lines_while p =
  let line = p <* optional eol >>| fun s -> s ^ "\n" in
  many1 line

let lines_starts_with p = lines_while ((spaces *> p <* spaces) *> optional_line)

let lines_till p = many_till (line <* optional eol) p

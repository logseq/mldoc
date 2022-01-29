open Angstrom
open Prelude

let whitespace_chars = [ ' '; '\t'; '\n'; '\r'; '\012' ]

let is_whitespace c = c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = '\012'

let space_chars = [ ' '; '\t'; '\026'; '\012' ]

module CharSet = Set.Make (Char)

let md_escape_chars =
  "!\"#$%&'()*+,-./:;<=>?@[]^_`{|}~\\" |> explode |> CharSet.of_list

let is_md_escape_char c = CharSet.mem c md_escape_chars

let is_space c = List.mem c space_chars

let is_tab = function
  | '\t' -> true
  | _ -> false

let is_tab_or_space = is_space

let non_tab_or_space = not << is_tab_or_space

let non_space = not << is_space

let eol_chars = [ '\r'; '\n' ]

let is_eol c = List.mem c eol_chars

let non_eol = not << is_eol

let non_space_eol c = non_space c && non_eol c

let is_space_eol c = is_space c || is_eol c

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

let peek_spaces_or_tabs = tabs_or_ws |> unsafe_lookahead

let take_till1 f = take_while1 (fun c -> not (f c))

let line = take_till1 is_eol

let optional_line = take_till is_eol

let line_without_spaces = take_till1 (fun c -> c = '\r' || c = '\n' || c = ' ')

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

let one_of cl = satisfy (fun c -> List.mem c cl)

let not_one_of cl = satisfy (fun c -> not (List.mem c cl))

let take_while1_include_backslash chars_can_escape f =
  let last_backslash = ref false in
  take_while1 (fun c ->
      if !last_backslash && List.mem c chars_can_escape then (
        last_backslash := false;
        true
      ) else if !last_backslash then (
        last_backslash := false;
        f c
      ) else if c = '\\' then (
        last_backslash := true;
        true
      ) else
        f c)

let page_ref, page_ref_ignore_bracket =
  (* allow single char ']' in pagename but "]]" *)
  let page_name_part =
    take_while1_include_backslash [ ']' ] (fun c -> non_eol c && c <> ']')
    <|> ( available >>= fun len ->
          if len < 2 then
            fail "page_name_part"
          else
            peek_string 2 >>= fun s ->
            if is_eol s.[0] then
              fail "page_name_part2"
            else if s = "]]" then
              fail "page_name_part3"
            else
              return s >>= fun _ -> any_char >>| String.make 1 )
  in
  let page_name =
    fix (fun m -> List.cons <$> page_name_part <*> m <|> return [])
    >>| String.concat ""
    >>= fun s ->
    if String.length s = 0 then
      fail "page_name"
    else
      return s
  in
  let p = list [ string "[["; page_name; string "]]" ] in
  (p >>| String.concat "", p >>| fun l -> List.nth l 1)

let block_ref, block_ref_ignore_bracket =
  let p =
    list
      [ string "(("
      ; take_while1 (function
          | ')' -> false
          | _ -> true)
      ; string "))"
      ]
  in
  (p >>| String.concat "", p >>| fun l -> List.nth l 1)

let any_char_string = String.make 1 <$> any_char

let string_contains_balanced_brackets ?(escape_chars = [])
    ?(excluded_ending_chars = []) bracket_pair other_delims =
  let left, right = unzip bracket_pair in
  fix (fun (m : string list list t) ->
      choice
        [ (fun s l -> [ List.cons s (List.flatten l) ])
          <$> take_while1_include_backslash escape_chars (fun c ->
                  (not @@ List.mem c other_delims)
                  && (not @@ List.mem c excluded_ending_chars)
                  && (not (List.mem c left))
                  && not (List.mem c right))
          <*> m
        ; ( peek_char >>= fun c ->
            match c with
            | None -> fail "finish"
            | Some c when List.mem c left ->
              (fun left l right -> [ [ left ]; List.flatten l; right ])
              <$> any_char_string <*> m
              <*> (char (List.assoc c bracket_pair)
                  >>= (fun c ->
                        (fun right l -> List.cons right (List.flatten l))
                        <$> return (String.make 1 c)
                        <*> m)
                  <|> return [])
            | Some c when List.mem c excluded_ending_chars ->
              available >>= fun len ->
              if len < 2 then
                fail "finish"
              else
                peek_string 2 >>= fun s ->
                let s1 = s.[1] in
                if List.mem s1 other_delims then
                  fail "finish"
                else
                  (fun c l -> [ [ c ]; List.flatten l ])
                  <$> any_char_string <*> m
            | Some _ -> fail "delims" )
        ; return [ [] ]
        ])
  >>| (String.concat "" << List.flatten)

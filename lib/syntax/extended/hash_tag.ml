open! Prelude
open Angstrom
open Parsers

let tag_delims = [ ','; ';'; '.'; '!'; '?'; '\''; '"'; ':' ]

let hashtag_name =
  let hashtag_name_part =
    take_while1 (fun c ->
        non_space_eol c && (not (List.mem c tag_delims)) && c <> '[' && c <> '#')
    <|> page_ref
    (* ignore last consecutive periods *)
    <|> ( take_while (fun c -> List.mem c tag_delims)
          *> (satisfy is_space_eol *> return () <|> end_of_input)
          *> return `Finish
        <|> (String.make 1 <$> not_one_of [ '#'; ','; '!'; '?'; '\''; '"'; ':' ] >>| fun c -> `Continue c)
        >>= fun r ->
          match r with
          | `Finish -> fail "hashtag_name_part finish"
          | `Continue c -> return c )
  in
  fix (fun m ->
      List.cons <$> hashtag_name_part <*> m
      <|> (List.cons <$> hashtag_name_part <*> return []))
  >>| String.concat ""

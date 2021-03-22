open Angstrom
open Parsers

let tag_delims = [ ','; ';'; '.'; '!'; '?' ]

let hashtag_name =
  let hashtag_name_part =
    take_while1 (fun c ->
        non_space_eol c && (not (List.mem c tag_delims)) && c <> '[')
    <|> page_ref
    <|> ( available >>= fun len ->
          if len < 2 then
            fail "hashtag_name_part"
          else
            peek_string 2 >>= fun s ->
            let s0, s1 = (s.[0], s.[1]) in
            if is_space s0 || is_eol s0 then
              fail "hashtag_name_part2"
            else if List.mem s0 tag_delims && (is_space s1 || is_eol s1) then
              fail "hashtag_name_part3"
            else
              any_char >>| String.make 1 )
  in
  fix (fun m ->
      List.cons <$> hashtag_name_part <*> m
      <|> (List.cons <$> hashtag_name_part <*> return []))
  >>| String.concat ""

(* TODO: how to express begin of line? Otherwise the tag should be prefixed by spaces *)
let parse = char '#' *> hashtag_name

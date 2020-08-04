open Prelude
open Angstrom
open Parsers

(* TODO: attributes check *)
(* TODO: quickcheck test, the output should be equal to the exactly input *)

let known_tags =
  ["a"; "abbr"; "address"; "area"; "article"; "aside"; "audio";
   "b"; "base"; "bdi"; "bdo"; "blockquote"; "body"; "br"; "button";
   "canvas"; "caption"; "cite"; "code"; "col"; "colgroup";
   "data"; "datalist"; "dd"; "del"; "dfn"; "div"; "dl"; "dt";
   "em"; "embed";
   "fieldset"; "figcaption"; "figure"; "footer"; "form";
   "h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "head"; "header"; "hr"; "html";
   "i"; "iframe"; "img"; "input"; "ins";
   "kbd"; "keygen";
   "label"; "legend"; "li"; "link";
   "main"; "map"; "mark"; "meta"; "meter";
   "nav"; "noscript";
   "object"; "ol"; "optgroup"; "option"; "output";
   "p"; "param"; "pre"; "progress";
   "q";
   "rb"; "rp"; "rt"; "rtc"; "ruby";
   "s"; "samp"; "script"; "section"; "select"; "small"; "source"; "span"; "strong"; "style"; "sub"; "sup";
   "table"; "tbody"; "td"; "template"; "textarea"; "tfoot"; "th"; "thead"; "time"; "title"; "tr"; "track";
   "u"; "ul";
   "var"; "video"; "details"; "summary"
   "wbr"]

let known_tag s =
  let s = String.lowercase_ascii s in
  List.mem s known_tags

let match_tag tag open_tag close_tag left_char =
  let level_ref = ref (1) in
  let s_ref = ref "" in
  fix (fun parse ->
      end_string_2 close_tag ~ci:true (fun s ->
          let level_without_attrs = count_substring s open_tag in
          let level_with_attrs = count_substring s (left_char ^ tag ^ " ") in
          let level = level_without_attrs + level_with_attrs in
          let _ = Printf.printf "Html tag level: %d, tag: %s, content: %s\n" level tag s in
          let _ = level_ref := !level_ref + level - 1 in
          let _ = s_ref := !s_ref ^ s ^ close_tag in
          if !level_ref <= 0 then
            return (left_char ^ tag ^ !s_ref)
          else
            parse))

let self_close_tag tag =
  end_string "/>" (fun s ->
      String.concat "" ["<"; tag; s; "/>"])

let tag_wrapper =
  char '<' *> take_till1 (fun c -> is_space c || c == '>')
  >>= fun tag ->
  (if known_tag tag then
     return tag
   else
     fail ("html invalid tag: " ^ tag))
  >>= fun tag ->
  let open_tag = "<" ^ tag ^ ">" in
  let close_tag = "</" ^ tag ^ ">" in
  match_tag tag open_tag close_tag "<"
  <|>
  self_close_tag tag

(* Idea and part of the code from Omd *)
let parse =
  (peek_string 10) >>= fun s ->
  match explode s with
  | '<' :: '?' :: _ ->
    between_string_strict_wrapper "<?" "?>"
  (* <!--> *)
  | '<' :: '!' :: '-' :: '-' :: _ ->
    between_string_strict_wrapper "<!--" "-->"
  (* <![CDATA[ ... ]]> *)
  | '<' :: '!' :: '[' :: 'C' :: 'D' :: 'A' :: 'T' :: 'A' :: '[' :: _ ->
    between_string_strict_wrapper "<![CDATA[" "]]"
  (* <!DOCTYPE html> *)
  | '<' :: '!' :: _ ->
    between_string_strict_wrapper "<!" ">"
  | '<' :: _ ->
    tag_wrapper
  | _ ->
    fail "raw html"

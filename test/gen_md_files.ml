open QCheck.Gen
open Mldoc

type state = { mutable last_level : int }

let page_ref_g (pagenames : string list) =
  oneofl pagenames >|= fun pagename ->
  Inline.Link
    { url = Inline.Search pagename
    ; label = [ Inline.Plain "" ]
    ; title = None
    ; full_text = Printf.sprintf "[[%s]]" pagename
    ; metadata = ""
    }

let unicode_table =
  ["¡"; "¢"; "£"; "¤"; "¥"; "¦"; "§"; "¨"; "©"; "ª"; "«"; "¬"; "­"; "®"; "¯"; "°"; "±"; "²"; "³"; "´"; "µ"; "¶"; "·"; "¸"; "¹"; "º"; "»"; "¼"; "½"; "¾"; "¿"; "À"; "Á"; "Â"; "Ã"; "Ä"; "Å"; "Æ"; "Ç"; "È"; "É"; "Ê"; "Ë"; "Ì"; "Í"; "Î"; "Ï"; "Ð"; "Ñ"; "Ò"; "Ó"; "Ô"; "Õ"; "Ö"; "×"; "Ø"; "Ù"; "Ú"; "Û"; "Ü"; "Ý"; "Þ"; "ß"; "à"; "á"; "â"; "ã"; "ä"; "å"; "æ"; "ç"; "è"; "é"; "ê"; "ë"; "ì"; "í"; "î"; "ï"; "ð"; "ñ"; "ò"; "ó"; "ô"; "õ"; "ö"; "÷"; "ø"; "ù"; "ú"; "û"; "ü"; "ý"; "þ"; "ÿ";] [@ocamlformat "disable"]

let char_table =
  [' '; '!'; '"'; '#'; '$'; '%'; '&'; '\''; '('; ')'; '*'; '+'; ','; '-'; '.';
  (* '/'; *) '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; ':'; ';'; '<'; '=';
  '>'; '?'; '@'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L';
  'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'; '[';
  '\\'; ']'; '^'; '_'; '`'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j';
  'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y';
  'z'; '{'; '|'; '}'; (* '\n' *)] [@ocamlformat "disable"]

let page_names n =
  let pagename i =
    let+ pagename_l =
      list_size (1 -- 10)
      @@ frequency
           [ (1, string_size ~gen:(oneofl char_table) (0 -- 5))
           ; (1, oneofl unicode_table)
           ]
    in
    String.(concat "" pagename_l ^ string_of_int i)
  in
  List.map (fun i -> generate1 (pagename i)) (List.init n (( + ) 1))

let dir_names = page_names

let plain_inline_g =
  string_size ~gen:(char_range 'A' 'Z') (1 -- 100) >|= fun s -> Inline.Plain s

let inline_g pagenames =
  frequency [ (10, plain_inline_g); (1, page_ref_g pagenames) ]

let inlines_g pagenames = list_size (1 -- 10) (inline_g pagenames)

let level_candidates max =
  let max' = (max + 3) / 4 in
  List.init max' (fun i -> ((i + 1) * 4) - 3)

let heading ?(init = false) pagenames state =
  let marker_g =
    frequencyl
      [ (1, Some "TODO"); (1, Some "DOING"); (1, Some "DONE"); (7, None) ]
  in
  let level_g =
    oneofl
      (if init then
        [ 1 ]
      else
        level_candidates (state.last_level + 4))
  in
  inlines_g pagenames >>= fun inlines ->
  marker_g >>= fun marker ->
  level_g >>= fun level ->
  state.last_level <- level;
  return
  @@ Type.Heading
       { title = inlines
       ; tags = []
       ; marker
       ; level
       ; numbering = None
       ; priority = None
       ; anchor = ""
       ; meta = { timestamps = []; properties = [] }
       ; unordered = true
       }

let paragragh pagenames =
  inlines_g pagenames >|= fun inlines -> Type.Paragraph inlines

let blocks_g pagenames : Type.blocks t =
  let state = { last_level = 1 } in
  let dummy_pos : Type.pos_meta = { start_pos = 0; end_pos = 0 } in
  let* init_heading = heading ~init:true pagenames state in
  let init_heading_with_pos = (init_heading, dummy_pos) in
  let block_with_pos_g =
    frequency [ (3, paragragh pagenames); (1, heading pagenames state) ]
    >|= fun b -> (b, dummy_pos)
  in
  let* blocks' = list_size (10 -- 500) block_with_pos_g in
  return (init_heading_with_pos :: blocks')

let document_g pagename pagenames =
  let* blocks = blocks_g pagenames in
  let title = pagename in
  return
    ({ filename = Some title
     ; blocks
     ; directives = []
     ; title = Some title
     ; subtitle = None
     ; author = None
     ; toc = []
     }
      : Document.t)

let config : Conf.t =
  { toc = true
  ; heading_number = true
  ; keep_line_break = false
  ; format = Conf.Markdown
  ; heading_to_list = true
  ; exporting_keep_properties = false
  ; inline_type_with_pos = false
  ; export_md_indent_style = Dashes
  }

(* ./gen_md_files.exe <page-num> <dir-num> *)
let _ =
  let argv = Array.to_list Sys.argv in
  let page_num =
    List.nth_opt argv 1
    |> Option.map_default (fun n -> Option.default 1 (int_of_string_opt n)) 1
  in
  let dir_num =
    List.nth_opt argv 2
    |> Option.map_default (fun n -> Option.default 1 (int_of_string_opt n)) 1
  in
  let exporter = Exporters.find "markdown" in
  let pagenames = page_names page_num in
  let dirnames = dir_names dir_num in
  List.iter (fun dirname -> Unix.mkdir dirname 0o777) dirnames;
  List.iter
    (fun pagename ->
      let doc = generate1 (document_g pagename pagenames) in
      let filepath =
        generate1
          ( oneofl dirnames >|= fun dirname ->
            Printf.sprintf "%s/%s.md" dirname pagename )
      in
      let chan = open_out filepath in
      Exporters.run exporter ~refs:None config doc chan)
    pagenames

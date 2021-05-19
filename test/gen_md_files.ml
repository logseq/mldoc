open QCheck.Gen
open Mldoc

type state = { mutable last_level : int }

let page_ref_g n =
  1 -- n >|= fun n ->
  Inline.Link
    { url = Inline.Search ("page-" ^ string_of_int n)
    ; label = [ Inline.Plain "" ]
    ; title = None
    ; full_text = Printf.sprintf "[[page-%d]]" n
    ; metadata = ""
    }

let plain_inline_g =
  string_size ~gen:(char_range 'A' 'Z') (1 -- 100) >|= fun s -> Inline.Plain s

let inline_g n = frequency [ (10, plain_inline_g); (1, page_ref_g n) ]

let inlines_g n = list_size (1 -- 10) (inline_g n)

let level_candidates max =
  let max' = (max + 3) / 4 in
  List.init max' (fun i -> ((i + 1) * 4) - 3)

let heading ?(init = false) n state =
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
  inlines_g n >>= fun inlines ->
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

let paragragh n = inlines_g n >|= fun inlines -> Type.Paragraph inlines

let blocks_g n : Type.blocks t =
  let state = { last_level = 1 } in
  let dummy_pos : Type.pos_meta = { start_pos = 0; end_pos = 0 } in
  let* init_heading = heading ~init:true n state in
  let init_heading_with_pos = (init_heading, dummy_pos) in
  let block_with_pos_g =
    frequency [ (3, paragragh n); (1, heading n state) ] >|= fun b ->
    (b, dummy_pos)
  in
  let* blocks' = list_size (10 -- 500) block_with_pos_g in
  return (init_heading_with_pos :: blocks')

let document_g index n =
  let* blocks = blocks_g n in
  let title = Printf.sprintf "page-%d" index in
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
  }

let _ =
  let page_num =
    List.nth_opt (Array.to_list Sys.argv) 1
    |> Option.map_default (fun n -> Option.default 1 (int_of_string_opt n)) 1
  in
  let exporter = Exporters.find "markdown" in
  List.iter
    (fun index ->
      let doc = generate1 (document_g index page_num) in
      let chan = open_out (Printf.sprintf "page-%d.md" index) in
      Exporters.run exporter ~refs:None config doc chan)
    (List.init page_num (( + ) 1))

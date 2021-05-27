open Prelude
open Conf

let default_state = Markdown.default_state

let empty_references = Reference.empty_parsed_t

(* TODO: delete me *)
let debug_config =
  { toc = true
  ; heading_number = true
  ; keep_line_break = false
  ; format = Conf.Markdown
  ; heading_to_list = true
  ; exporting_keep_properties = false
  }

let rec block_tree_to_plain_tree (blocks : Tree_type.value) : string Zip.l =
  let open Zip in
  match blocks with
  | Leaf (t, _pos) ->
    leaf
      Markdown.(
        to_string debug_config
        @@ block empty_references (default_state ()) debug_config t)
  | Branch [] -> Branch []
  | Branch l -> branch @@ CCList.map block_tree_to_plain_tree l

let attr ?(uri = "") local value = ((uri, local), value)

let outline_frag ?(childs = []) text : string Zip.l Xmlm.frag =
  let attrs = [ attr "text" text ] in
  `El ((("", "outline"), attrs), childs)

let rec plain_tree_to_frag (plain_tree : string Zip.l) : string Zip.l Xmlm.frag
    =
  let open Zip in
  match plain_tree with
  | Leaf s -> outline_frag s
  | Branch [] -> `Data ""
  | Branch (Leaf h :: t) -> outline_frag ~childs:t h
  | Branch [ (Branch __ as e) ] -> plain_tree_to_frag e
  | Branch (Branch _ :: _ as l) -> outline_frag ~childs:l ""

(* TODO: delete me *)
let debug_output buf = Xmlm.make_output ~indent:(Some 2) (`Buffer buf)

let output_tree_type o tree =
  Xmlm.output_doc_tree plain_tree_to_frag o (None, tree)

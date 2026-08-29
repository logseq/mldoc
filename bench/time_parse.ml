(** Wall-clock timing focused on Markdown (Logseq) workloads. *)
open Mldoc.Parser
open Mldoc.Conf

let ensure_logseq_large path =
  if not (Sys.file_exists path) then (
    let buf = Buffer.create 1_200_000 in
    for i = 0 to 3999 do
      Buffer.add_string buf
        (Printf.sprintf
           "- Block title %d with [[page %d]] and #tag%d\n" i (i mod 50)
           (i mod 20));
      if i mod 3 = 0 then
        Buffer.add_string buf
          (Printf.sprintf "  id:: %08x-xxxx-xxxx-xxxx-%012x\n" i i);
      if i mod 5 = 0 then (
        Buffer.add_string buf
          (Printf.sprintf
             "  - child of %d with ((%08x-xxxx-xxxx-xxxx-%012x))\n" i i i);
        Buffer.add_string buf
          (Printf.sprintf "    more plain text line without markup %d\n" i));
      if i mod 7 = 0 then
        Buffer.add_string buf
          (Printf.sprintf
             "  plain paragraph under block %d word word word word word\n" i);
      if i mod 11 = 0 then
        Buffer.add_string buf (Printf.sprintf "  + unordered item %d\n" i);
      if i mod 13 = 0 then
        Buffer.add_string buf (Printf.sprintf "  ```\n  code line %d\n  ```\n" i)
    done;
    let content = Buffer.contents buf in
    (* Grow to ~1.2MB so benches stay comparable across revisions. *)
    let pieces = ref [] in
    let len = ref 0 in
    while !len < 1_200_000 do
      pieces := content :: !pieces;
      len := !len + String.length content
    done;
    let grown = String.concat "" (List.rev !pieces) in
    let oc = open_out path in
    output_string oc (String.sub grown 0 1_200_000);
    close_out oc)

let () = ensure_logseq_large "./examples/logseq_large.md"

let doc_org = load_file "./examples/doc.org"
let syntax_md = load_file "./examples/syntax.md"
let logseq_md = load_file "./examples/logseq_large.md"

let base =
  { toc = true
  ; parse_outline_only = false
  ; heading_number = true
  ; keep_line_break = false
  ; format = Markdown
  ; heading_to_list = false
  ; exporting_keep_properties = false
  ; inline_type_with_pos = false
  ; inline_skip_macro = false
  ; export_md_indent_style = Dashes
  ; export_md_remove_options = []
  ; hiccup_in_block = true
  ; enable_drawers = true
  ; parse_marker = true
  ; parse_priority = true
  }

let avg ~n f =
  ignore (f ());
  let t0 = Unix.gettimeofday () in
  for _ = 1 to n do
    ignore (f ())
  done;
  let t1 = Unix.gettimeofday () in
  (t1 -. t0) /. float n

let () =
  let n = 3 in
  let md_full = avg ~n (fun () -> parse base logseq_md) in
  let md_outline =
    avg ~n (fun () -> parse { base with parse_outline_only = true } logseq_md)
  in
  let syn_full = avg ~n (fun () -> parse base syntax_md) in
  let syn_outline =
    avg ~n (fun () -> parse { base with parse_outline_only = true } syntax_md)
  in
  let org = { base with format = Org; parse_outline_only = false } in
  let org_full = avg ~n (fun () -> parse org doc_org) in
  let org_outline =
    avg ~n (fun () -> parse { org with parse_outline_only = true } doc_org)
  in
  Printf.printf "iterations=%d (avg seconds)\n" n;
  Printf.printf "MD logseq_large full:         %.4f\n" md_full;
  Printf.printf "MD logseq_large outline_only: %.4f  (%.1fx vs full)\n" md_outline
    (md_full /. md_outline);
  Printf.printf "MD syntax.md full:            %.4f\n" syn_full;
  Printf.printf "MD syntax.md outline_only:    %.4f  (%.1fx vs full)\n" syn_outline
    (syn_full /. syn_outline);
  Printf.printf "Org doc.org full:             %.4f\n" org_full;
  Printf.printf "Org doc.org outline_only:     %.4f  (%.1fx vs full)\n" org_outline
    (org_full /. org_outline)

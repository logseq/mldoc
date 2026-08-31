(** Bench parsing a Logseq Markdown graph directory (many small files). *)
open Mldoc.Parser

open Mldoc.Conf

let rec md_files acc dir =
  match Unix.opendir dir with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> acc
  | dh ->
    let rec loop acc =
      match Unix.readdir dh with
      | exception End_of_file ->
        Unix.closedir dh;
        acc
      | "."
      | ".." ->
        loop acc
      | name ->
        let path = Filename.concat dir name in
        let acc =
          match Unix.stat path with
          | { st_kind = S_DIR; _ } -> md_files acc path
          | { st_kind = S_REG; _ } when Filename.check_suffix name ".md" ->
            path :: acc
          | _ -> acc
        in
        loop acc
    in
    loop acc

let load_all dir =
  let files = md_files [] dir in
  let files = List.sort String.compare files in
  List.map (fun path -> (path, load_file path)) files

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

let property_values contents =
  let acc = ref [] in
  List.iter
    (fun content ->
      String.split_on_char '\n' content
      |> List.iter (fun line ->
             match String.index_opt line ':' with
             | Some i when i + 1 < String.length line && line.[i + 1] = ':' ->
               let v =
                 String.trim
                   (String.sub line (i + 2) (String.length line - i - 2))
               in
               if v <> "" then acc := v :: !acc
             | _ -> ()))
    contents;
  List.rev !acc

let () =
  let dir =
    if Array.length Sys.argv > 1 then
      Sys.argv.(1)
    else
      "/tmp/ls-movies-4k"
  in
  let pages = Filename.concat dir "pages" in
  let journals = Filename.concat dir "journals" in
  let loaded =
    if Sys.file_exists pages then
      load_all pages
      @
      if Sys.file_exists journals then
        load_all journals
      else
        []
    else
      load_all dir
  in
  let n_files = List.length loaded in
  let contents = List.map snd loaded in
  let bytes = List.fold_left (fun s c -> s + String.length c) 0 contents in
  let concat = String.concat "\n" contents in
  let values = property_values contents in
  let n = 3 in
  Printf.printf "graph=%s files=%d bytes=%d avg_file=%.0f props=%d\n" dir
    n_files bytes
    (if n_files = 0 then
       0.
     else
       float bytes /. float n_files)
    (List.length values);
  let parse_each config docs =
    List.iter (fun c -> ignore (parse config c)) docs
  in
  let full = avg ~n (fun () -> parse_each base contents) in
  let outline =
    avg ~n (fun () ->
        parse_each { base with parse_outline_only = true } contents)
  in
  let concat_full = avg ~n (fun () -> parse base concat) in
  let concat_outline =
    avg ~n (fun () -> parse { base with parse_outline_only = true } concat)
  in
  let refs =
    avg ~n (fun () ->
        List.iter
          (fun v -> ignore (Mldoc.Property.property_references base v))
          values)
  in
  Printf.printf "iterations=%d (avg seconds)\n" n;
  Printf.printf "per-file full:              %.4f  (%.1f files/s)\n" full
    (float n_files /. full);
  Printf.printf "per-file outline_only:      %.4f  (%.1fx vs full)\n" outline
    (full /. outline);
  Printf.printf "concatenated full:          %.4f\n" concat_full;
  Printf.printf "concatenated outline_only:  %.4f  (%.1fx vs concat full)\n"
    concat_outline
    (concat_full /. concat_outline);
  Printf.printf "property_references only:   %.4f  (%.0f%% of per-file full)\n"
    refs
    (100. *. refs /. full)

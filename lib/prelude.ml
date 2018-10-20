let (<<) f g x = f(g(x))

let identity x = x

(* list *)
let remove p = List.filter (fun x -> not (p x))

let take n l =
  let rec loop n acc = function
    | h :: t when n > 0 ->
      loop (n - 1) (h :: acc) t
    | _ ->
      List.rev acc
  in
  loop n [] l

let drop_last n l =
  let len = List.length l in
  let rec loop n acc = function
    | h :: t when n > 0 ->
      loop (n - 1) (h :: acc) t
    | _ ->
      List.rev acc
  in
  loop (len - n) [] l

let filter_map f l =
  let rec loop dst = function
    | [] -> List.rev dst
    | h :: t ->
      match f h with
      | None -> loop dst t
      | Some x ->
        loop (x :: dst) t
  in
  loop [] l

let print_list l =
  List.iter print_endline l

(* string *)
let is_ordered s =
  Str.string_match (Str.regexp "[0-9]+\\.") s 0

let get_ordered_number s =
  try Scanf.sscanf s "%d. " (fun i -> Some i) with _ -> None

exception Found_int of int

let get_indent line =
  let len = String.length line in
  try
    for i = 0 to len - 1 do
      if line.[i] <> ' ' then raise (Found_int i)
    done;
    0
  with Found_int i ->
    i

(* TODO: only used in dev profile. *)
let time f =
  let t = Sys.time () in
  let result = f () in
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t) ;
  result

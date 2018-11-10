let (<<) f g x = f(g(x))

let identity x = x

(* list *)
let hd_opt = function
  | h :: [] -> Some h
  | _ -> None

let rec last_opt = function
  | [] -> None
  | h :: [] -> Some h
  | h :: tl -> last_opt tl

let remove p = List.filter (fun x -> not (p x))

let join separator l =
  let rec aux acc = function
    | [] -> acc
    | h :: [] ->
      h :: acc
    | h :: t ->
      h :: (separator :: aux acc t) in
  aux [] l

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

let print_bool = function
  | true -> print_string "true"
  | _ -> print_string "false"

(* string *)
let starts_with s check =
  let open String in
  if length s >= length check then
    if String.lowercase_ascii (sub s 0 (length check)) = String.lowercase_ascii check then
      true
    else
      false
  else
    false

let splitl p str =
  let len = String.length str in
  let i = ref 0 in
  while !i < len && p str.[!i] do incr i; done;
  String.sub str 0 !i, String.sub str !i (len - !i)

let splitr p str =
  let len = String.length str in
  let i = ref len in
  while !i > 0 && p str.[!i - 1] do decr i; done;
  String.sub str 0 !i, String.sub str !i (len - !i)

let is_uppercase c = 'A' <= c && c <= 'Z'
let is_lowercase c = 'a' <= c && c <= 'z'
let is_letter c =
  is_uppercase c || is_lowercase c

let is_digit = function '0' .. '9' -> true | _ -> false

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let is_ordered s =
  let chars = explode s in
  List.for_all (fun c -> is_digit c) (drop_last 1 chars)
  &&
  String.get s (String.length s - 1) = '.'

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

let result_default default = function
  | Ok result -> result
  | Error e -> default

let lines s = String.split_on_char '\n' s

let clear_indents s =
  List.map String.trim (lines s)

let change_ext ext file =
  if file = "-" then file
  else Filename.chop_extension file ^ "." ^ ext

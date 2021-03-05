let ( << ) f g x = f (g x)

(* TODO: group using modules *)

let identity x = x

(* list *)

let repeat n x =
  let rec go acc = function
    | 0 -> acc
    | n when n >= 1 -> go (x :: acc) (n - 1)
    | _ -> raise (Invalid_argument "List.repeat")
  in
  go [] n

let hd_opt = function
  | h :: _ -> Some h
  | _ -> None

let rec last_opt = function
  | [] -> None
  | [ h ] -> Some h
  | _ :: tl -> last_opt tl

let rec last = function
  | [] -> raise Not_found
  | [ h ] -> h
  | _ :: tl -> last tl

let remove p = List.filter (fun x -> not (p x))

let join separator l =
  let rec aux acc = function
    | [] -> acc
    | [ h ] -> h :: acc
    | h :: t -> h :: separator :: aux acc t
  in
  aux [] l

let take n l =
  let rec loop n acc = function
    | h :: t when n > 0 -> loop (n - 1) (h :: acc) t
    | _ -> List.rev acc
  in
  loop n [] l

let drop_last n l =
  let len = List.length l in
  let rec loop n acc = function
    | h :: t when n > 0 -> loop (n - 1) (h :: acc) t
    | _ -> List.rev acc
  in
  loop (len - n) [] l

let split_n n l =
  if List.length l < n then
    (l, [ 0 ])
  else
    let rec loop n acc = function
      | h :: t when n > 0 -> loop (n - 1) (h :: acc) t
      | _ -> (List.rev acc, l)
    in
    loop n [] l

let filter_map f l =
  let rec loop dst = function
    | [] -> List.rev dst
    | h :: t -> (
      match f h with
      | None -> loop dst t
      | Some x -> loop (x :: dst) t)
  in
  loop [] l

let unzip l =
  List.fold_right (fun (a, b) (al, bl) -> (a :: al, b :: bl)) l ([], [])

let print_list l = List.iter print_endline l

let print_bool = function
  | true -> print_string "true"
  | _ -> print_string "false"

(* array *)
module Array = struct
  include Array

  let findi p xs =
    let n = Array.length xs in
    let rec loop i =
      if i = n then
        raise Not_found
      else if p xs.(i) then
        i
      else
        loop (succ i)
    in
    loop 0
end

(* string *)
let starts_with s check =
  let open String in
  if length s >= length check then
    if
      String.lowercase_ascii (sub s 0 (length check))
      = String.lowercase_ascii check
    then
      true
    else
      false
  else
    false

let ends_with s check =
  let open String in
  let len = length s in
  let len_c = length check in
  if len >= len_c then
    if
      String.lowercase_ascii (sub s (len - len_c) len_c)
      = String.lowercase_ascii check
    then
      true
    else
      false
  else
    false

let splitl p str =
  let len = String.length str in
  let i = ref 0 in
  while !i < len && p str.[!i] do
    incr i
  done;
  (String.sub str 0 !i, String.sub str !i (len - !i))

let splitr p str =
  let len = String.length str in
  let i = ref len in
  while !i > 0 && p str.[!i - 1] do
    decr i
  done;
  (String.sub str 0 !i, String.sub str !i (len - !i))

let lchop ?(n = 1) s =
  if n < 0 then
    invalid_arg "String.lchop: number of characters to chop is negative"
  else
    let slen = String.length s in
    if slen <= n then
      ""
    else
      String.sub s n (slen - n)

let is_uppercase c = 'A' <= c && c <= 'Z'

let is_lowercase c = 'a' <= c && c <= 'z'

let is_letter c = is_uppercase c || is_lowercase c

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_letter_or_digit c = is_letter c || is_digit c

let explode s =
  let rec exp i l =
    if i < 0 then
      l
    else
      exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

let is_number s =
  let chars = explode s in
  List.for_all (fun c -> is_digit c) chars

let last_char s =
  if String.length s > 0 then
    let c = s.[String.length s - 1] in
    Some c
  else
    None

let is_ordered s =
  let chars = explode s in
  List.for_all (fun c -> is_digit c) (drop_last 1 chars)
  && last_char s = Some '.'

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
  with Found_int i -> i

let result_default default = function
  | Ok result -> result
  | Error _e -> default

let lines s = String.split_on_char '\n' s

let change_ext ext file =
  if file = "-" then
    file
  else
    Filename.chop_extension file ^ "." ^ ext

let rec remove_dups lst =
  match lst with
  | [] -> []
  | h :: t -> h :: remove_dups (List.filter (fun x -> x <> h) t)

let split_first c s =
  try
    let k = String.index s c in
    let n = String.length s in
    (String.sub s 0 k, String.sub s (k + 1) (n - k - 1))
  with _ -> (s, "")

let safe_sub s ofs len =
  if String.length s > ofs && len >= 0 then
    String.sub s ofs len
  else
    s

let count_substring str sub =
  let sub_len = String.length sub in
  let len_diff = String.length str - sub_len
  and reg = Str.regexp_string sub in
  let rec aux i n =
    if i > len_diff then
      n
    else
      try
        let pos = Str.search_forward reg str i in
        aux (pos + sub_len) (succ n)
      with Not_found -> n
  in
  aux 0 0

(* Copied from https://github.com/c-cube/ocaml-containers/blob/master/src/core/CCList.ml *)
module CCList = struct
  (* max depth for direct recursion *)
  let direct_depth_default_ = 1000

  let tail_map f l =
    (* Unwind the list of tuples, reconstructing the full list front-to-back.
       @param tail_acc a suffix of the final list; we append tuples' content
       at the front of it *)
    let rec rebuild tail_acc = function
      | [] -> tail_acc
      | (y0, y1, y2, y3, y4, y5, y6, y7, y8) :: bs ->
        rebuild
          (y0 :: y1 :: y2 :: y3 :: y4 :: y5 :: y6 :: y7 :: y8 :: tail_acc)
          bs
    in
    (* Create a compressed reverse-list representation using tuples
       @param tuple_acc a reverse list of chunks mapped with [f] *)
    let rec dive tuple_acc = function
      | x0 :: x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: xs ->
        let y0 = f x0 in
        let y1 = f x1 in
        let y2 = f x2 in
        let y3 = f x3 in
        let y4 = f x4 in
        let y5 = f x5 in
        let y6 = f x6 in
        let y7 = f x7 in
        let y8 = f x8 in
        dive ((y0, y1, y2, y3, y4, y5, y6, y7, y8) :: tuple_acc) xs
      | xs ->
        (* Reverse direction, finishing off with a direct map *)
        let tail = List.map f xs in
        rebuild tail tuple_acc
    in
    dive [] l

  let map f l =
    let rec direct f i l =
      match l with
      | [] -> []
      | [ x ] -> [ f x ]
      | [ x1; x2 ] ->
        let y1 = f x1 in
        [ y1; f x2 ]
      | [ x1; x2; x3 ] ->
        let y1 = f x1 in
        let y2 = f x2 in
        [ y1; y2; f x3 ]
      | _ when i = 0 -> tail_map f l
      | x1 :: x2 :: x3 :: x4 :: l' ->
        let y1 = f x1 in
        let y2 = f x2 in
        let y3 = f x3 in
        let y4 = f x4 in
        y1 :: y2 :: y3 :: y4 :: direct f (i - 1) l'
    in
    direct f direct_depth_default_ l

  let direct_depth_append_ = 10_000

  let append l1 l2 =
    let rec direct i l1 l2 =
      match l1 with
      | [] -> l2
      | _ when i = 0 -> safe l1 l2
      | x :: l1' -> x :: direct (i - 1) l1' l2
    and safe l1 l2 = List.rev_append (List.rev l1) l2 in
    match l1 with
    | [] -> l2
    | [ x ] -> x :: l2
    | [ x; y ] -> x :: y :: l2
    | _ -> direct direct_depth_append_ l1 l2

  let direct_depth_filter_ = 10_000

  let filter p l =
    let rec direct i p l =
      match l with
      | [] -> []
      | _ when i = 0 -> safe p l []
      | x :: l' when not (p x) -> direct i p l'
      | x :: l' -> x :: direct (i - 1) p l'
    and safe p l acc =
      match l with
      | [] -> List.rev acc
      | x :: l' when not (p x) -> safe p l' acc
      | x :: l' -> safe p l' (x :: acc)
    in
    direct direct_depth_filter_ p l
end

let ( @ ) = List.append

let clear_indents s = CCList.map String.trim (lines s)

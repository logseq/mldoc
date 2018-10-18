let (<<) f g x = f(g(x))

let remove p = List.filter (fun x -> not (p x))

let identity x = x

(* list *)

let take n l =
  let rec loop n acc = function
    | h :: t when n > 0 ->
      loop (n - 1) (h :: acc) t
    | _ ->
      List.rev acc
  in
  loop n [] l

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

(* TODO: only used in dev profile. *)
let time f =
  let t = Sys.time () in
  let result = f () in
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t) ;
  result

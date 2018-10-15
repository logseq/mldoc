let (<<) f g x = f(g(x))

let remove p = List.find_all (fun x -> not (p x))

(* TODO: only used in dev profile. *)
let time f =
  let t = Sys.time () in
  let result = f () in
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t) ;
  result

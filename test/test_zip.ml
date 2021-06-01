open Zip
open QCheck

let g =
  Gen.(
    of_l
    <$> sized
        @@ fix (fun self n ->
               match n with
               | 0 -> map leaf nat
               | n ->
                 frequency
                   [ ( 5
                     , branch
                       <$> ( 0 -- (n / 2) >>= fun n' ->
                             list_size (0 -- 10) (self n') ) )
                   ; (1, map leaf nat)
                   ]))

let arbitrary = make g

(* sum zipper using next  *)
let sum_by_next z =
  let rec aux z r =
    match (node z, is_end z) with
    | Leaf n, false -> aux (next z) (r + n)
    | Leaf _, true -> r
    | Branch _, false -> aux (next z) r
    | Branch _, true -> r
  in
  aux z 0

(* sum zipper using prev *)
let sum_by_prev z =
  let rec to_end z =
    let z' = next z in
    if is_end z' then
      z
    else
      to_end z'
  in
  let rec aux z r =
    let r' =
      match node z with
      | Leaf n -> r + n
      | Branch _ -> r
    in
    match prev z with
    | Some z' -> aux z' r'
    | None -> r'
  in
  aux (to_end z) 0

let sum z =
  let value = root z in
  let rec aux value =
    match value with
    | Leaf n -> n
    | Branch l -> List.fold_left (fun r e -> r + aux e) 0 l
  in
  aux value

let next_prev_sum_test =
  Test.make ~name:"next_prev_sum_generative_test" ~count:1000 arbitrary
    (fun z ->
      let sum' = sum z in
      sum_by_next z = sum' && sum_by_prev z = sum')

let () =
  Alcotest.run "zip"
  @@ [ ("next & prev", [ QCheck_alcotest.to_alcotest next_prev_sum_test ]) ]

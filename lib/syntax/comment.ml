open Angstrom
open Parsers
open Type

let parse =
  let p =
    char '#' *>
    peek_char >>= function
    | None ->
      return [Comment ""]
    | Some c ->
      if is_space c || is_eol c then
        optional line >>= fun line ->
        let line = match line with None -> "" | Some s -> s in
        return [Comment line]
      else
        fail "comment"
  in between_eols p

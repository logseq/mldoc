open Angstrom
open Parsers

(* copied from https://github.com/dune-universe/dune-universe/blob/c7009f6f606b52b0c7a44841084009d9743f2246/packages/email_message.v0.13.0/email_address/src/email_address_parser_stable_v1.ml *)

(* Ignore prefix for now. *)
type t =
  { local_part : string
  ; domain : string
  }
[@@deriving yojson]

let string_contains = String.contains

let whitespace_chars = " \r\n\t"

let not_address_chars = "<>@," ^ whitespace_chars

let not_domain_chars = not_address_chars ^ "'\""

let address_part =
  let local_part =
    take_while1 (fun chr -> not (string_contains not_address_chars chr))
    <?> "local_part"
  in
  let domain =
    char '@'
    *> take_while1 (fun chr -> not (string_contains not_domain_chars chr))
    <?> "domain"
  in
  lift2 (fun local_part domain -> { domain; local_part }) local_part domain

let email =
  optional (char '<') *> address_part <* optional (char '>') <?> "email"

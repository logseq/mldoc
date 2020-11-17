type t = {start: Timestamp.t; stop: Timestamp.t} [@@deriving yojson]

let to_string {start; stop} =
  Printf.sprintf "%s--%s" (Timestamp.to_string start) (Timestamp.to_string stop)

type pos_meta =
  { start_pos : int
  ; end_pos : int
  }
[@@deriving yojson]

let dummy_pos = { start_pos = 0; end_pos = 0 }

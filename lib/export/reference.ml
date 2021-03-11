type t =
  { (* (block-uuid, (content, title)) list *)
    embed_blocks : (string * (string * Inline.t list)) list
  ; (* (page-name, content) list *)
    embed_pages : (string * string) list
  }
[@@deriving yojson]

type parsed_t =
  { parsed_embed_blocks : (string * (Type.t list * Inline.t list)) list
  ; parsed_embed_pages : (string * Type.t list) list
  }

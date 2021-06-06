type t =
  { (* (block-uuid, (content-include-children, content)) list *)
    embed_blocks : (string * (string * string)) list
  ; (* (page-name, content) list *)
    embed_pages : (string * string) list
  }
[@@deriving yojson]

type parsed_t =
  { (** (block-uuid, (content-include-children, content)) list *)
    parsed_embed_blocks : (string * (Type.t list * Type.t list)) list
  ; (** (page-name, content) list *)
    parsed_embed_pages : (string * Type.t list) list
  }

let empty_parsed_t = { parsed_embed_blocks = []; parsed_embed_pages = [] }

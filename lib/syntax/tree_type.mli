type t

type value = Type.t_with_pos_meta Zip.l

val of_blocks : Type.blocks -> t

val of_blocks_without_pos : Type.t list -> t

val to_value : t -> value

val replace_embed_and_refs : t -> Reference.parsed_t -> t

val to_blocks : t -> Type.blocks

val to_blocks_without_pos : t -> Type.t list

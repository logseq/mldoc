type 'a t = 'a Zip.t

type value = Type.t_with_pos_meta Zip.l

type value_with_content = Type.t_with_content Zip.l

val of_blocks : Type.blocks -> Type.t_with_pos_meta t

val of_blocks_without_pos : Type.t list -> Type.t_with_pos_meta t

val of_value : value -> Type.t_with_pos_meta t

val of_value_with_content : value_with_content -> Type.t_with_content t

val to_value : Type.t_with_pos_meta t -> value

val to_blocks : Type.t_with_pos_meta t -> Type.blocks

val to_blocks_without_pos : Type.t_with_pos_meta t -> Type.t list

val to_blocks_with_content : Type.t_with_content t -> Type.blocks_with_content

val remove_properties : Type.t t -> Type.t t

val replace_embed_and_refs :
  Type.t_with_pos_meta t -> refs:Reference.parsed_t -> Type.t_with_pos_meta t

val replace_heading_with_paragraph :
  Type.t_with_pos_meta t -> Type.t_with_pos_meta t

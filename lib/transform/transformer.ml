open! Prelude

module Property : sig
  val remove_properties_on_type : Type.t list -> Type.t list

  val remove_properties_on_tree_type : Type.t Tree_type.t -> Type.t Tree_type.t
end = struct
  let remove_properties_on_type = Type_op.remove_properties

  let remove_properties_on_tree_type = Tree_type.remove_properties
end

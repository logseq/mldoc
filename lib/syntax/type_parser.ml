open! Prelude

module rec Lists : sig
  val parse :
    Conf.t -> (Type.t * Pos.pos_meta) list Angstrom.t -> Type.t Angstrom.t
end =
  Lists0.MakeLists (Heading)

and Heading : sig
  val parse : Conf.t -> Type.t Angstrom.t

  (** (level, is_unordered, size) **)
  val level : Conf.t -> (int * bool * int option) Angstrom.t

  val marker : string Angstrom.t

  val priority : char Angstrom.t

  val anchor_link : string -> string
end =
  Heading0.MakeHeading (Block)

and Block : sig
  val parse : Conf.t -> Type.t Angstrom.t

  val fenced_code_block : Type.t Angstrom.t

  val results : Type.t Angstrom.t
end =
  Block0.MakeBlock (Lists)

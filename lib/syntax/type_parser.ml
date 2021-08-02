open! Prelude

module rec Lists : sig
  val parse :
    Conf.t -> (Type.t * Pos.pos_meta) list Angstrom.t -> Type.t Angstrom.t
end =
  Lists0.MakeLists (Heading)

and Heading : sig
  val parse : Conf.t -> Type.t Angstrom.t

  val anchor_link : string -> string
end =
  Heading0.MakeHeading (Lists) (Block)

and Block : sig
  val parse : Conf.t -> Type.t Angstrom.t

  val results : Type.t Angstrom.t
end =
  Block0.MakeBlock (Lists)

(** Entity definition *)

(** This short files defines the entities of org.
    Note that latin1 part of entities have been suppressed. *)
type t =
  { name: string
  ; latex: string
  ; latex_mathp: bool
  ; html: string
  ; ascii: string
  ; unicode: string }

val find : string -> t
(** [find name] finds the entity named [name] or raises [Not_found] *)

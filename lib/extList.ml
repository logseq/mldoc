module type ExtList = sig
  type elt

  val push : elt -> unit

  val get : unit -> elt list

  val update : (elt list -> elt list) -> unit
end

module Make (S : sig
  type t

  val base : t list
end) : ExtList with type elt = S.t = struct
  type elt = S.t

  let r = ref S.base

  let push x = r := x :: !r

  let get () = !r

  let update f = r := f !r
end

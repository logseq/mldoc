module type Exporter = sig
  val name : string
  (* val config : Config.t *)

  val default_filename : string -> string

  (* val export: Config.t -> Document.t -> 'a output -> unit *)
  val export: Document.t -> out_channel -> unit
end

type exporter = (module Exporter)

let find name =
  List.find (fun m ->
      let module M = (val m : Exporter) in
      M.name = name )

module Exporters = struct
  include ExtList.Make (struct
      type t = exporter

      let base = []
    end)

  let run exporter doc output =
    let module M = (val exporter : Exporter) in
    M.export doc output

  let find name = find name (get ())

  let add = push
end

(* comment part *)

let generate backend doc output =
  let export = Exporters.find backend in
  Exporters.run export doc output

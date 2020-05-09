(* taken from mlorg *)
module type Exporter = sig
  val name : string

  val default_filename : string -> string

  val export: Conf.t -> Document.t -> out_channel -> unit
end

type exporter = (module Exporter)

let find name =
  List.find (fun m ->
      let module M = (val m : Exporter) in
      M.name = name )

module Exporters = struct
  include ExtList.Make (struct
      type t = exporter

      let base = [(module Html.HtmlExporter : Exporter)]
    end)

  let run exporter config doc output =
    let module M = (val exporter : Exporter) in
    M.export config doc output

  let find name = find name (get ())

  let add = push
end

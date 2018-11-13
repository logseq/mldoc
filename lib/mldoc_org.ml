(** Entry point of the org library *)

module Document = Document
module Block = Block
module Inline = Inline
module Exporters = Exporter.Exporters
module Timestamp = Timestamp
module Parser = Org_parser
module Type = Type
module Backends = struct
  module Html = Html
end

module Xml = Xml

(* TODO:
   - unordered list support "*".
   - ordered list support other formats, not just numbers.
   - unescape
   - Code block supports reference (name attribute).
   - table support column groups
   - configuration, image extensions
*)

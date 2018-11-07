type meta =
  { timestamps: Inline.timestamp list
  ; footnotes: (string * Inline.t list) list
  (** The footnotes defined in that heading *)
  ; properties: (string * string) list  (** The properties of the heading *)}

type heading =
  { title: Inline.t list
  ; level: int
  ; tags: string list
  ; marker: string option
  ; priority: char option  (** The optional priority *)
  ; content: Org.blocks
  ; children: heading list
  ; meta: meta
  ; anchor: string }

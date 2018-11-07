type meta =
  { timestamps: Timestamp.t list
  (** The plain timestamps appearing in the heading *)
  ; ranges: Timestamp.range list
  (** The timestamp ranges  appearing in the heading *)
  ; scheduled: Timestamp.t list
  (** The SCHEDULED item appearing in the heading *)
  ; deadline: Timestamp.t list (** The deadlines appearing in the heading *)
  ; clocks: Timestamp.range list  (** The clocked amount of time *)
  ; current_clock: Timestamp.t list
  (** The optional time when it was clocked *)
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

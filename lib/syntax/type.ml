type inline_list = Inline.t list [@@deriving yojson]
type pos_meta = { start_pos: int
                ; end_pos: int} [@@deriving yojson]

type heading =
  { title: inline_list  (** The title as inline formatted content *)
  ; tags: string list  (** The tags set by the user *)
  ; marker: string option  (** TODO, DONE, and so on *)
  ; level: int  (** The level (number of stars) -- starts at 1 *)
  ; numbering: int list option
  ; priority: char option  (** The optional priority *)
  ; anchor: string
  ; meta: meta
  } [@@deriving yojson]
and meta =
  { timestamps: Inline.timestamp list
  ; properties: (string * string) list  (** The properties of the heading *)} [@@deriving yojson]

and list_item =
  { content: t list  (** The contents of the current item *)
  ; items: list_item list
  ; number: int option  (** Its number *)
  ; name: Inline.t list  (** Definition name *)
  ; checkbox: bool option  (** Was it checked *)
  ; indent: int (** Indentation of the current item. *)
  ; ordered: bool} [@@deriving yojson]

and table = { header: row option
            ; groups: group list (* rows groups *)
            ; col_groups: int list} [@@deriving yojson]
and
  group = row list
and
  row = col list
and
  col = Inline.t list

(** {2 Code blocks} *)
and code_block =
  { lines: string list
  ; language: string option (** The language the code is written in *)
  ; options: string list option
  } [@@deriving yojson]
(** Code blocks *)

and t =
    Paragraph of Inline.t list  (** A paragraph containing only inline text *)
  | Paragraph_Sep
  | Heading of heading  (** A heading *)
  | List of list_item list  (** A list [item] *)
  | Directive of string * string  (** A directive [name, value] *)
  | Math of string  (** Math, enclosed by $$ ... $$ *)
  | With_Keywords of (string * string) list * t  (** Keywords for a block *)
  (* blocks *)
  | Results                     (* TODO: include content or not? *)
  | Example of string list
  (** [Examples] used to typeset random code snippets. The integer is the line number in the source file. *)
  | Src of code_block
  (** [Src] is used to typeset code snippets. The integer is the line number in the source file. *)
  | Quote of t list  (** Quoted text *)
  | Export of string * string list option * string
  | CommentBlock of string list
  | Custom of string * string option * t list * string
  (** Custom block of the form
      #+begin_name opts
      DATA
      #+end *)
  | Latex_Fragment of Inline.latex_fragment
  | Latex_Environment of string * string option * string
  (** Latex environment. Of the form
      {v \begin{foo}
      bar
      \end{foo} v}
  *)
  | Drawer of string * string list  (** A drawer *)
  | Property_Drawer of (string * string) list (** A property drawer *)
  | Footnote_Definition of string * Inline.t list
  (** The definition of a footnote : name and contents *)
  | Horizontal_Rule  (** Horizontal rule *)
  | Table of table  (** A block *)
  | Comment of string           (** Comment *)
  | Raw_Html of string
  | Hiccup of string
[@@deriving yojson]

and t_with_pos_meta = t * pos_meta [@@deriving yojson]
and blocks = t_with_pos_meta list [@@deriving yojson]

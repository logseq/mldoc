type heading =
  { title: Inline.t list  (** The title as inline formatted content *)
  ; tags: string list  (** The tags set by the user *)
  ; marker: string option  (** TODO, DONE, and so on *)
  ; level: int  (** The level (number of stars) -- starts at 1 *)
  ; priority: char option  (** The optional priority *) }

and list_item =
  { content: string list  (** The contents of the current item *)
  (* ; items: t list *)
  ; items: list_item list
  ; number: int option  (** Its number *)
  ; checkbox: bool option  (** Was it checked *)
  ; indent: int (** Indentation of the current item. *)
  ; ordered: bool
  }

and table = { header: row option
            ; groups: group list}
and
  group = row list
and
  row = col list
and
  col = Inline.t list

(** {2 Code blocks} *)
and code_block =
  { numbering: [`Yes | `Keep] option  (** Is it to be numbered ? *)
  ; lines: (string * string option) list
  (** The contents as a list of (lines, ref to that line) *)
  ; ref_format: 'a 'b. (string -> 'a, 'b, 'a, 'a) format4
  (** The format used to parse ref *)
  (* ; header_arguments: Hd_arguments.t  (\** The header arguments *\) *)
  ; language: string  (** The language the code is written in *)
  ; linenumber: int  (** The line number it starts at *) }
(** Code blocks *)

and t =
  | Paragraph of Inline.t list  (** A paragraph containing only inline text *)
  | Heading of heading  (** A heading *)
  | List of list_item list  (** A list [item] *)
  | Directive of string * string  (** A directive [name, value] *)
  | Math of string  (** Math, enclosed by $$ ... $$ *)
  | Quote of t list  (** Quoted text *)
  | With_Keywords of (string * string) list * t  (** Keywords for a block *)
  | Example of int * string list
  (** [Examples] used to typeset random code snippets. The integer is the line number in the source file. *)
  | Src of code_block
  (** [Src] is used to typeset code snippets. The integer is the line number in the source file. *)
  | Custom of string * string * t list
  (** Custom block of the form
      #+begin_name opts
      DATA
      #+end *)
  | Latex_Environment of string * string * string list
  (** Latex environment. Of the form
      {v \begin{foo}
      bar
      \end{foo} v}
  *)
  | Drawer of string * t list  (** A drawer *)
  | Property_Drawer of (string * string) list  (** A property drawer *)
  | Footnote_Definition of string * Inline.t list
  (** The definition of a footnote : name and contents *)
  | Horizontal_Rule  (** Horizontal rule *)
  | Table of table  (** A block *)

and blocks = t list

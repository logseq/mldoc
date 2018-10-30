type list_item =
  { contents: t list  (** The item's contents *)
  ; number: string option  (** The optional number of the item *)
  ; checkbox: bool option
  (** Does it have a checkbox ([[ ]]) and is it checked ? *) }

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
  | Heading of Heading.t  (** A heading *)
  | List of list_item list * bool  (** A list [item, ordered?] *)
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
  | Table of Table.t  (** A block *)

and blocks = t list

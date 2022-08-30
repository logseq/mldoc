type inline_list = Inline.t_with_pos list [@@deriving yojson]

type inline_list_no_pos = Inline.t list [@@deriving yojson]

type heading =
  { title : inline_list  (** The title as inline formatted content *)
  ; tags : string list  (** The tags set by the user *)
  ; marker : string option [@default None]  (** TODO, DONE, and so on *)
  ; level : int  (** The level (number of stars) -- starts at 1 *)
  ; numbering : int list option [@default None]
  ; priority : char option [@default None]  (** The optional priority *)
  ; anchor : string
  ; meta : meta
  ; unordered : bool  (** whether it's an unordered list (starts with `-`) **)
  ; size : int option
  }
[@@deriving yojson]

and meta =
  { timestamps : Inline.timestamp list
  ; properties : (string * string * Inline.t list) list  (** The properties of the heading *)
  }
[@@deriving yojson]

and list_item =
  { content : t list  (** The contents of the current item *)
  ; items : list_item list
  ; number : int option [@default None]  (** Its number *)
  ; name : Inline.t_with_pos list  (** Definition name *)
  ; checkbox : bool option [@default None]  (** Was it checked *)
  ; indent : int  (** Indentation of the current item. *)
  ; ordered : bool
  }
[@@deriving yojson]

and table =
  { header : row option [@default None]
  ; groups : group list (* rows groups *)
  ; col_groups : int list
  }
[@@deriving yojson]

and group = row list

and row = col list

and col = Inline.t list

(** {2 Code blocks} *)
and code_block =
  { lines : string list
  ; language : string option [@default None]
        (** The language the code is written in *)
  ; options : string list option [@default None]
  ; pos_meta : Pos.pos_meta
  }
[@@deriving yojson]
(** Code blocks *)

and t =
  | Paragraph of Inline.t_with_pos list
      (** A paragraph containing only inline text *)
  | Paragraph_line of string  (** Internal usage *)
  | Paragraph_Sep of int
  | Heading of heading  (** A heading *)
  | List of list_item list  (** A list [item] *)
  | Directive of string * string  (** A directive [name, value] *)
  (* blocks *)
  | Results (* TODO: include content or not? *)
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
  | Displayed_Math of string
  (* FIXME:  *)
  | Drawer of string * string list  (** A drawer *)
  | Property_Drawer of (string * string * Inline.t list) list  (** A property drawer *)
  | Footnote_Definition of string * Inline.t_with_pos list
      (** The definition of a footnote : name and contents *)
  | Horizontal_Rule  (** Horizontal rule *)
  | Table of table  (** A block *)
  | Comment of string  (** Comment *)
  | Raw_Html of string
  | Hiccup of string
[@@deriving yojson]

and t_with_pos_meta = t * Pos.pos_meta [@@deriving yojson]

and t_with_content = t * string [@@deriving yojson]

and blocks = t_with_pos_meta list [@@deriving yojson]

and blocks_with_content = t_with_content list [@@deriving yojson]

(* and blocks = t list [@@deriving yojson] *)

let pp fmt t =
  Format.pp_print_string fmt @@ Yojson.Safe.pretty_to_string @@ to_yojson t

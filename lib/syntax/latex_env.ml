(*
You can insert LaTeX blocks by:

: $$Your big formulas$$

eg,

$$ \sum_{k=1}^{+\infty} \frac 1 {k^2} = \frac{\pi^2} 6$$

or you can insert plain LaTeX environment

: \begin{env}options
: Contents
: \end{env}

     \begin{equation}
     x=\sqrt{b}
     \end{equation}

     If $a^2=b$ and \( b=2 \), then the solution must be
     either $$ a=+\sqrt{2} $$ or \[ a=-\sqrt{2} \].

Whenever possible, you should use [[Custom%20blocks][custom blocks]], that get exported to
latex environment in LaTeX-based outputs, and are more portable (in
HTML, they are exported as div that you can style or script).

   (* TODO: *)
\def\arraystretch{1.5}
   \begin{array}{c:c:c}
   a & b & c \\ \hline
   d & e & f \\
   \hdashline
   g & h & i
\end{array}

   x = \begin{cases}
   a &\text{if } b \\
   c &\text{if } d
\end{cases}
*)
open Angstrom
open Parsers
open Type
open! Prelude

let env_name_options_parser =
  string_ci "\\begin{" *> take_while1 (fun c -> c <> '}')
  <* char '}'
  >>| (fun name -> (name, None))
  <* spaces_or_eols

let parse _config =
  spaces *> env_name_options_parser >>= fun (name, options) ->
  let ending = "\\end{" ^ name ^ "}" in
  let ending_len = String.length ending in
  fix (fun m ->
      peek_char >>= fun c ->
      match c with
      | None -> return []
      | Some '\\' ->
        (* check if equals to '\end{...}' *)
        available >>= fun len ->
        if len < ending_len then
          fail "ending"
        else
          peek_string ending_len >>= fun s ->
          if String.lowercase_ascii ending = String.lowercase_ascii s then
            advance ending_len >>= fun _ -> return []
          else
            List.cons <$> (any_char >>| String.make 1) <*> m
      | Some _ -> List.cons <$> take_while1 (fun c -> c <> '\\') <*> m)
  >>| String.concat ""
  >>| fun content ->
  Latex_Environment (String.lowercase_ascii name, options, content)

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
*)
open Angstrom
open Parsers
open Type
open Prelude

let env_name_options_parser =
  lift2 (fun name options ->
      match options with
      | None | Some "" -> (name, None)
      | _ -> (name, options))
    (string_ci "\\begin{" *>
     (take_while1 (fun c -> c <> '}'))
     <* char '}')
    (optional line)
  <* end_of_line

let parse _config =
  spaces *> env_name_options_parser >>= fun (name, options) ->
  end_string ("\\end{" ^ name ^ "}") ~ci:true (fun s ->
      let lines = String.split_on_char '\n' s in
      [Latex_Environment (String.lowercase_ascii name, options, lines)])

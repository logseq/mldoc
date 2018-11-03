(*
You can insert LaTeX blocks by:

: $$Your big formulas$$

eg,

$$ \sum_{k=1}^{+\infty} \frac 1 {k^2} = \frac{\pi^2} 6$$

or you can insert plain LaTeX environment

: \begin{env}options
: Contents
: \end{env}

     \begin{equation}greatwork
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
open Org
open Prelude

let env_name_options_parser =
  lift2 (fun name options ->
      match options with
      | None | Some "" -> (name, None)
      | _ -> (name, options))
    (string_ci "\begin{" *>
     (take_while1 (fun c -> c <> '}'))
     <* char '}')
    (optional line)
  <* eol

let parse =
  optional eols *> optional ws *>
  peek_char_fail >>= function
  | '\b' ->                      (* block *)
    env_name_options_parser >>= fun (name, options) ->
    between_lines (fun line ->
        let prefix = "\\end{" ^ name ^ "}" in
        starts_with line prefix) "Latex_environment body"
    >>= (fun lines ->
        return [Org.Latex_Environment (String.lowercase_ascii name, options, lines)])
  | _ ->
    Inline.latex_fragment >>= function
    | Inline.Latex_Fragment x ->
      return [Org.Latex_Fragment x]
    | _ ->
      fail "Latex_env latex_fragment"

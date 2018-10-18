(*
You can insert LaTeX blocks by:

: $$Your big formulas$$

eg,

$$ \sum_{k=1}^{+\infty} \frac 1 {k^2} = \frac{\pi^2} 6$$

or you can insert plain LaTeX environment

: \begin{env}options
: Contents
: \end{env}

Whenever possible, you should use [[Custom%20blocks][custom blocks]], that get exported to
latex environment in LaTeX-based outputs, and are more portable (in
HTML, they are exported as div that you can style or script).
*)

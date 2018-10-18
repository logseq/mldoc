(*
Drawers a way to hide information in =org-mode=. The syntax is:
: :DRAWERNAME:
: Contents of the drawer (socks for instance)
: :END:

There is a special kind of drawer that =mlorg= recognizes, it is the
/PROPERTY/ drawer, which look like:
: :PROPERTIES:
: :KEY: Value
: :KEY: Value
: :KEY: Value
: :END:
They are used to store information about a heading and can be used to
filter on them. (Exporters don't use them as of today)
*)

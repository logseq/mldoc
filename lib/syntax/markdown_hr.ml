open Angstrom

let hr_char = choice [ char '-'; char '*'; char '_' ]

let parse = many1 hr_char

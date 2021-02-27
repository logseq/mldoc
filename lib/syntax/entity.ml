type t =
  { name : string
  ; latex : string
  ; latex_mathp : bool
  ; html : string
  ; ascii : string
  ; unicode : string
  }
[@@deriving yojson]

let data =
  [ { name = "Agrave"
    ; latex = "\\`{A}"
    ; latex_mathp = false
    ; html = "&Agrave;"
    ; ascii = "A"
    ; unicode = "À"
    }
  ; { name = "agrave"
    ; latex = "\\`{a}"
    ; latex_mathp = false
    ; html = "&agrave;"
    ; ascii = "a"
    ; unicode = "à"
    }
  ; { name = "Aacute"
    ; latex = "\\'{A}"
    ; latex_mathp = false
    ; html = "&Aacute;"
    ; ascii = "A"
    ; unicode = "Á"
    }
  ; { name = "aacute"
    ; latex = "\\'{a}"
    ; latex_mathp = false
    ; html = "&aacute;"
    ; ascii = "a"
    ; unicode = "á"
    }
  ; { name = "Acirc"
    ; latex = "\\^{A}"
    ; latex_mathp = false
    ; html = "&Acirc;"
    ; ascii = "A"
    ; unicode = "Â"
    }
  ; { name = "acirc"
    ; latex = "\\^{a}"
    ; latex_mathp = false
    ; html = "&acirc;"
    ; ascii = "a"
    ; unicode = "â"
    }
  ; { name = "Atilde"
    ; latex = "\\~{A}"
    ; latex_mathp = false
    ; html = "&Atilde;"
    ; ascii = "A"
    ; unicode = "Ã"
    }
  ; { name = "atilde"
    ; latex = "\\~{a}"
    ; latex_mathp = false
    ; html = "&atilde;"
    ; ascii = "a"
    ; unicode = "ã"
    }
  ; { name = "Auml"
    ; latex = "\\\"{A}"
    ; latex_mathp = false
    ; html = "&Auml;"
    ; ascii = "Ae"
    ; unicode = "Ä"
    }
  ; { name = "auml"
    ; latex = "\\\"{a}"
    ; latex_mathp = false
    ; html = "&auml;"
    ; ascii = "ae"
    ; unicode = "ä"
    }
  ; { name = "Aring"
    ; latex = "\\AA{}"
    ; latex_mathp = false
    ; html = "&Aring;"
    ; ascii = "A"
    ; unicode = "Å"
    }
  ; { name = "AA"
    ; latex = "\\AA{}"
    ; latex_mathp = false
    ; html = "&Aring;"
    ; ascii = "A"
    ; unicode = "Å"
    }
  ; { name = "aring"
    ; latex = "\\aa{}"
    ; latex_mathp = false
    ; html = "&aring;"
    ; ascii = "a"
    ; unicode = "å"
    }
  ; { name = "AElig"
    ; latex = "\\AE{}"
    ; latex_mathp = false
    ; html = "&AElig;"
    ; ascii = "AE"
    ; unicode = "Æ"
    }
  ; { name = "aelig"
    ; latex = "\\ae{}"
    ; latex_mathp = false
    ; html = "&aelig;"
    ; ascii = "ae"
    ; unicode = "æ"
    }
  ; { name = "Ccedil"
    ; latex = "\\c{C}"
    ; latex_mathp = false
    ; html = "&Ccedil;"
    ; ascii = "C"
    ; unicode = "Ç"
    }
  ; { name = "ccedil"
    ; latex = "\\c{c}"
    ; latex_mathp = false
    ; html = "&ccedil;"
    ; ascii = "c"
    ; unicode = "ç"
    }
  ; { name = "Egrave"
    ; latex = "\\`{E}"
    ; latex_mathp = false
    ; html = "&Egrave;"
    ; ascii = "E"
    ; unicode = "È"
    }
  ; { name = "egrave"
    ; latex = "\\`{e}"
    ; latex_mathp = false
    ; html = "&egrave;"
    ; ascii = "e"
    ; unicode = "è"
    }
  ; { name = "Eacute"
    ; latex = "\\'{E}"
    ; latex_mathp = false
    ; html = "&Eacute;"
    ; ascii = "E"
    ; unicode = "É"
    }
  ; { name = "eacute"
    ; latex = "\\'{e}"
    ; latex_mathp = false
    ; html = "&eacute;"
    ; ascii = "e"
    ; unicode = "é"
    }
  ; { name = "Ecirc"
    ; latex = "\\^{E}"
    ; latex_mathp = false
    ; html = "&Ecirc;"
    ; ascii = "E"
    ; unicode = "Ê"
    }
  ; { name = "ecirc"
    ; latex = "\\^{e}"
    ; latex_mathp = false
    ; html = "&ecirc;"
    ; ascii = "e"
    ; unicode = "ê"
    }
  ; { name = "Euml"
    ; latex = "\\\"{E}"
    ; latex_mathp = false
    ; html = "&Euml;"
    ; ascii = "E"
    ; unicode = "Ë"
    }
  ; { name = "euml"
    ; latex = "\\\"{e}"
    ; latex_mathp = false
    ; html = "&euml;"
    ; ascii = "e"
    ; unicode = "ë"
    }
  ; { name = "Igrave"
    ; latex = "\\`{I}"
    ; latex_mathp = false
    ; html = "&Igrave;"
    ; ascii = "I"
    ; unicode = "Ì"
    }
  ; { name = "igrave"
    ; latex = "\\`{i}"
    ; latex_mathp = false
    ; html = "&igrave;"
    ; ascii = "i"
    ; unicode = "ì"
    }
  ; { name = "Iacute"
    ; latex = "\\'{I}"
    ; latex_mathp = false
    ; html = "&Iacute;"
    ; ascii = "I"
    ; unicode = "Í"
    }
  ; { name = "iacute"
    ; latex = "\\'{i}"
    ; latex_mathp = false
    ; html = "&iacute;"
    ; ascii = "i"
    ; unicode = "í"
    }
  ; { name = "Icirc"
    ; latex = "\\^{I}"
    ; latex_mathp = false
    ; html = "&Icirc;"
    ; ascii = "I"
    ; unicode = "Î"
    }
  ; { name = "icirc"
    ; latex = "\\^{i}"
    ; latex_mathp = false
    ; html = "&icirc;"
    ; ascii = "i"
    ; unicode = "î"
    }
  ; { name = "Iuml"
    ; latex = "\\\"{I}"
    ; latex_mathp = false
    ; html = "&Iuml;"
    ; ascii = "I"
    ; unicode = "Ï"
    }
  ; { name = "iuml"
    ; latex = "\\\"{i}"
    ; latex_mathp = false
    ; html = "&iuml;"
    ; ascii = "i"
    ; unicode = "ï"
    }
  ; { name = "Ntilde"
    ; latex = "\\~{N}"
    ; latex_mathp = false
    ; html = "&Ntilde;"
    ; ascii = "N"
    ; unicode = "Ñ"
    }
  ; { name = "ntilde"
    ; latex = "\\~{n}"
    ; latex_mathp = false
    ; html = "&ntilde;"
    ; ascii = "n"
    ; unicode = "ñ"
    }
  ; { name = "Ograve"
    ; latex = "\\`{O}"
    ; latex_mathp = false
    ; html = "&Ograve;"
    ; ascii = "O"
    ; unicode = "Ò"
    }
  ; { name = "ograve"
    ; latex = "\\`{o}"
    ; latex_mathp = false
    ; html = "&ograve;"
    ; ascii = "o"
    ; unicode = "ò"
    }
  ; { name = "Oacute"
    ; latex = "\\'{O}"
    ; latex_mathp = false
    ; html = "&Oacute;"
    ; ascii = "O"
    ; unicode = "Ó"
    }
  ; { name = "oacute"
    ; latex = "\\'{o}"
    ; latex_mathp = false
    ; html = "&oacute;"
    ; ascii = "o"
    ; unicode = "ó"
    }
  ; { name = "Ocirc"
    ; latex = "\\^{O}"
    ; latex_mathp = false
    ; html = "&Ocirc;"
    ; ascii = "O"
    ; unicode = "Ô"
    }
  ; { name = "ocirc"
    ; latex = "\\^{o}"
    ; latex_mathp = false
    ; html = "&ocirc;"
    ; ascii = "o"
    ; unicode = "ô"
    }
  ; { name = "Otilde"
    ; latex = "\\~{O}"
    ; latex_mathp = false
    ; html = "&Otilde;"
    ; ascii = "O"
    ; unicode = "Õ"
    }
  ; { name = "otilde"
    ; latex = "\\~{o}"
    ; latex_mathp = false
    ; html = "&otilde;"
    ; ascii = "o"
    ; unicode = "õ"
    }
  ; { name = "Ouml"
    ; latex = "\\\"{O}"
    ; latex_mathp = false
    ; html = "&Ouml;"
    ; ascii = "Oe"
    ; unicode = "Ö"
    }
  ; { name = "ouml"
    ; latex = "\\\"{o}"
    ; latex_mathp = false
    ; html = "&ouml;"
    ; ascii = "oe"
    ; unicode = "ö"
    }
  ; { name = "Oslash"
    ; latex = "\\O"
    ; latex_mathp = false
    ; html = "&Oslash;"
    ; ascii = "O"
    ; unicode = "Ø"
    }
  ; { name = "oslash"
    ; latex = "\\o{}"
    ; latex_mathp = false
    ; html = "&oslash;"
    ; ascii = "o"
    ; unicode = "ø"
    }
  ; { name = "OElig"
    ; latex = "\\OE{}"
    ; latex_mathp = false
    ; html = "&OElig;"
    ; ascii = "OE"
    ; unicode = "Œ"
    }
  ; { name = "oelig"
    ; latex = "\\oe{}"
    ; latex_mathp = false
    ; html = "&oelig;"
    ; ascii = "oe"
    ; unicode = "œ"
    }
  ; { name = "Scaron"
    ; latex = "\\v{S}"
    ; latex_mathp = false
    ; html = "&Scaron;"
    ; ascii = "S"
    ; unicode = "Š"
    }
  ; { name = "scaron"
    ; latex = "\\v{s}"
    ; latex_mathp = false
    ; html = "&scaron;"
    ; ascii = "s"
    ; unicode = "š"
    }
  ; { name = "szlig"
    ; latex = "\\ss{}"
    ; latex_mathp = false
    ; html = "&szlig;"
    ; ascii = "ss"
    ; unicode = "ß"
    }
  ; { name = "Ugrave"
    ; latex = "\\`{U}"
    ; latex_mathp = false
    ; html = "&Ugrave;"
    ; ascii = "U"
    ; unicode = "Ù"
    }
  ; { name = "ugrave"
    ; latex = "\\`{u}"
    ; latex_mathp = false
    ; html = "&ugrave;"
    ; ascii = "u"
    ; unicode = "ù"
    }
  ; { name = "Uacute"
    ; latex = "\\'{U}"
    ; latex_mathp = false
    ; html = "&Uacute;"
    ; ascii = "U"
    ; unicode = "Ú"
    }
  ; { name = "uacute"
    ; latex = "\\'{u}"
    ; latex_mathp = false
    ; html = "&uacute;"
    ; ascii = "u"
    ; unicode = "ú"
    }
  ; { name = "Ucirc"
    ; latex = "\\^{U}"
    ; latex_mathp = false
    ; html = "&Ucirc;"
    ; ascii = "U"
    ; unicode = "Û"
    }
  ; { name = "ucirc"
    ; latex = "\\^{u}"
    ; latex_mathp = false
    ; html = "&ucirc;"
    ; ascii = "u"
    ; unicode = "û"
    }
  ; { name = "Uuml"
    ; latex = "\\\"{U}"
    ; latex_mathp = false
    ; html = "&Uuml;"
    ; ascii = "Ue"
    ; unicode = "Ü"
    }
  ; { name = "uuml"
    ; latex = "\\\"{u}"
    ; latex_mathp = false
    ; html = "&uuml;"
    ; ascii = "ue"
    ; unicode = "ü"
    }
  ; { name = "Yacute"
    ; latex = "\\'{Y}"
    ; latex_mathp = false
    ; html = "&Yacute;"
    ; ascii = "Y"
    ; unicode = "Ý"
    }
  ; { name = "yacute"
    ; latex = "\\'{y}"
    ; latex_mathp = false
    ; html = "&yacute;"
    ; ascii = "y"
    ; unicode = "ý"
    }
  ; { name = "Yuml"
    ; latex = "\\\"{Y}"
    ; latex_mathp = false
    ; html = "&Yuml;"
    ; ascii = "Y"
    ; unicode = "Ÿ"
    }
  ; { name = "yuml"
    ; latex = "\\\"{y}"
    ; latex_mathp = false
    ; html = "&yuml;"
    ; ascii = "y"
    ; unicode = "ÿ"
    }
  ; { name = "fnof"
    ; latex = "\\textit{f}"
    ; latex_mathp = false
    ; html = "&fnof;"
    ; ascii = "f"
    ; unicode = "ƒ"
    }
  ; { name = "real"
    ; latex = "\\Re"
    ; latex_mathp = true
    ; html = "&real;"
    ; ascii = "R"
    ; unicode = "ℜ"
    }
  ; { name = "image"
    ; latex = "\\Im"
    ; latex_mathp = true
    ; html = "&image;"
    ; ascii = "I"
    ; unicode = "ℑ"
    }
  ; { name = "weierp"
    ; latex = "\\wp"
    ; latex_mathp = true
    ; html = "&weierp;"
    ; ascii = "P"
    ; unicode = "℘"
    }
  ; { name = "Alpha"
    ; latex = "A"
    ; latex_mathp = false
    ; html = "&Alpha;"
    ; ascii = "Alpha"
    ; unicode = "Α"
    }
  ; { name = "alpha"
    ; latex = "\\alpha"
    ; latex_mathp = true
    ; html = "&alpha;"
    ; ascii = "alpha"
    ; unicode = "α"
    }
  ; { name = "Beta"
    ; latex = "B"
    ; latex_mathp = false
    ; html = "&Beta;"
    ; ascii = "Beta"
    ; unicode = "Β"
    }
  ; { name = "beta"
    ; latex = "\\beta"
    ; latex_mathp = true
    ; html = "&beta;"
    ; ascii = "beta"
    ; unicode = "β"
    }
  ; { name = "Gamma"
    ; latex = "\\Gamma"
    ; latex_mathp = true
    ; html = "&Gamma;"
    ; ascii = "Gamma"
    ; unicode = "Γ"
    }
  ; { name = "gamma"
    ; latex = "\\gamma"
    ; latex_mathp = true
    ; html = "&gamma;"
    ; ascii = "gamma"
    ; unicode = "γ"
    }
  ; { name = "Delta"
    ; latex = "\\Delta"
    ; latex_mathp = true
    ; html = "&Delta;"
    ; ascii = "Delta"
    ; unicode = "Δ"
    }
  ; { name = "delta"
    ; latex = "\\delta"
    ; latex_mathp = true
    ; html = "&delta;"
    ; ascii = "delta"
    ; unicode = "δ"
    }
  ; { name = "Epsilon"
    ; latex = "E"
    ; latex_mathp = false
    ; html = "&Epsilon;"
    ; ascii = "Epsilon"
    ; unicode = "Ε"
    }
  ; { name = "epsilon"
    ; latex = "\\epsilon"
    ; latex_mathp = true
    ; html = "&epsilon;"
    ; ascii = "epsilon"
    ; unicode = "ε"
    }
  ; { name = "varepsilon"
    ; latex = "\\varepsilon"
    ; latex_mathp = true
    ; html = "&epsilon;"
    ; ascii = "varepsilon"
    ; unicode = "ε"
    }
  ; { name = "Zeta"
    ; latex = "Z"
    ; latex_mathp = false
    ; html = "&Zeta;"
    ; ascii = "Zeta"
    ; unicode = "Ζ"
    }
  ; { name = "zeta"
    ; latex = "\\zeta"
    ; latex_mathp = true
    ; html = "&zeta;"
    ; ascii = "zeta"
    ; unicode = "ζ"
    }
  ; { name = "Eta"
    ; latex = "H"
    ; latex_mathp = false
    ; html = "&Eta;"
    ; ascii = "Eta"
    ; unicode = "Η"
    }
  ; { name = "eta"
    ; latex = "\\eta"
    ; latex_mathp = true
    ; html = "&eta;"
    ; ascii = "eta"
    ; unicode = "η"
    }
  ; { name = "Theta"
    ; latex = "\\Theta"
    ; latex_mathp = true
    ; html = "&Theta;"
    ; ascii = "Theta"
    ; unicode = "Θ"
    }
  ; { name = "theta"
    ; latex = "\\theta"
    ; latex_mathp = true
    ; html = "&theta;"
    ; ascii = "theta"
    ; unicode = "θ"
    }
  ; { name = "thetasym"
    ; latex = "\\vartheta"
    ; latex_mathp = true
    ; html = "&thetasym;"
    ; ascii = "theta"
    ; unicode = "ϑ"
    }
  ; { name = "vartheta"
    ; latex = "\\vartheta"
    ; latex_mathp = true
    ; html = "&thetasym;"
    ; ascii = "theta"
    ; unicode = "ϑ"
    }
  ; { name = "Iota"
    ; latex = "I"
    ; latex_mathp = false
    ; html = "&Iota;"
    ; ascii = "Iota"
    ; unicode = "Ι"
    }
  ; { name = "iota"
    ; latex = "\\iota"
    ; latex_mathp = true
    ; html = "&iota;"
    ; ascii = "iota"
    ; unicode = "ι"
    }
  ; { name = "Kappa"
    ; latex = "K"
    ; latex_mathp = false
    ; html = "&Kappa;"
    ; ascii = "Kappa"
    ; unicode = "Κ"
    }
  ; { name = "kappa"
    ; latex = "\\kappa"
    ; latex_mathp = true
    ; html = "&kappa;"
    ; ascii = "kappa"
    ; unicode = "κ"
    }
  ; { name = "Lambda"
    ; latex = "\\Lambda"
    ; latex_mathp = true
    ; html = "&Lambda;"
    ; ascii = "Lambda"
    ; unicode = "Λ"
    }
  ; { name = "lambda"
    ; latex = "\\lambda"
    ; latex_mathp = true
    ; html = "&lambda;"
    ; ascii = "lambda"
    ; unicode = "λ"
    }
  ; { name = "Mu"
    ; latex = "M"
    ; latex_mathp = false
    ; html = "&Mu;"
    ; ascii = "Mu"
    ; unicode = "Μ"
    }
  ; { name = "mu"
    ; latex = "\\mu"
    ; latex_mathp = true
    ; html = "&mu;"
    ; ascii = "mu"
    ; unicode = "μ"
    }
  ; { name = "nu"
    ; latex = "\\nu"
    ; latex_mathp = true
    ; html = "&nu;"
    ; ascii = "nu"
    ; unicode = "ν"
    }
  ; { name = "Nu"
    ; latex = "N"
    ; latex_mathp = false
    ; html = "&Nu;"
    ; ascii = "Nu"
    ; unicode = "Ν"
    }
  ; { name = "Xi"
    ; latex = "\\Xi"
    ; latex_mathp = true
    ; html = "&Xi;"
    ; ascii = "Xi"
    ; unicode = "Ξ"
    }
  ; { name = "xi"
    ; latex = "\\xi"
    ; latex_mathp = true
    ; html = "&xi;"
    ; ascii = "xi"
    ; unicode = "ξ"
    }
  ; { name = "Omicron"
    ; latex = "O"
    ; latex_mathp = false
    ; html = "&Omicron;"
    ; ascii = "Omicron"
    ; unicode = "Ο"
    }
  ; { name = "omicron"
    ; latex = "\\textit{o}"
    ; latex_mathp = false
    ; html = "&omicron;"
    ; ascii = "omicron"
    ; unicode = "ο"
    }
  ; { name = "Pi"
    ; latex = "\\Pi"
    ; latex_mathp = true
    ; html = "&Pi;"
    ; ascii = "Pi"
    ; unicode = "Π"
    }
  ; { name = "pi"
    ; latex = "\\pi"
    ; latex_mathp = true
    ; html = "&pi;"
    ; ascii = "pi"
    ; unicode = "π"
    }
  ; { name = "Rho"
    ; latex = "P"
    ; latex_mathp = false
    ; html = "&Rho;"
    ; ascii = "Rho"
    ; unicode = "Ρ"
    }
  ; { name = "rho"
    ; latex = "\\rho"
    ; latex_mathp = true
    ; html = "&rho;"
    ; ascii = "rho"
    ; unicode = "ρ"
    }
  ; { name = "Sigma"
    ; latex = "\\Sigma"
    ; latex_mathp = true
    ; html = "&Sigma;"
    ; ascii = "Sigma"
    ; unicode = "Σ"
    }
  ; { name = "sigma"
    ; latex = "\\sigma"
    ; latex_mathp = true
    ; html = "&sigma;"
    ; ascii = "sigma"
    ; unicode = "σ"
    }
  ; { name = "sigmaf"
    ; latex = "\\varsigma"
    ; latex_mathp = true
    ; html = "&sigmaf;"
    ; ascii = "sigmaf"
    ; unicode = "ς"
    }
  ; { name = "varsigma"
    ; latex = "\\varsigma"
    ; latex_mathp = true
    ; html = "&sigmaf;"
    ; ascii = "varsigma"
    ; unicode = "ς"
    }
  ; { name = "Tau"
    ; latex = "T"
    ; latex_mathp = false
    ; html = "&Tau;"
    ; ascii = "Tau"
    ; unicode = "Τ"
    }
  ; { name = "Upsilon"
    ; latex = "\\Upsilon"
    ; latex_mathp = true
    ; html = "&Upsilon;"
    ; ascii = "Upsilon"
    ; unicode = "Υ"
    }
  ; { name = "upsih"
    ; latex = "\\Upsilon"
    ; latex_mathp = true
    ; html = "&upsih;"
    ; ascii = "upsilon"
    ; unicode = "ϒ"
    }
  ; { name = "upsilon"
    ; latex = "\\upsilon"
    ; latex_mathp = true
    ; html = "&upsilon;"
    ; ascii = "upsilon"
    ; unicode = "υ"
    }
  ; { name = "Phi"
    ; latex = "\\Phi"
    ; latex_mathp = true
    ; html = "&Phi;"
    ; ascii = "Phi"
    ; unicode = "Φ"
    }
  ; { name = "phi"
    ; latex = "\\phi"
    ; latex_mathp = true
    ; html = "&phi;"
    ; ascii = "phi"
    ; unicode = "φ"
    }
  ; { name = "Chi"
    ; latex = "X"
    ; latex_mathp = false
    ; html = "&Chi;"
    ; ascii = "Chi"
    ; unicode = "Χ"
    }
  ; { name = "chi"
    ; latex = "\\chi"
    ; latex_mathp = true
    ; html = "&chi;"
    ; ascii = "chi"
    ; unicode = "χ"
    }
  ; { name = "acutex"
    ; latex = "\\acute x"
    ; latex_mathp = true
    ; html = "&acute;x"
    ; ascii = "'x"
    ; unicode = "𝑥́"
    }
  ; { name = "Psi"
    ; latex = "\\Psi"
    ; latex_mathp = true
    ; html = "&Psi;"
    ; ascii = "Psi"
    ; unicode = "Ψ"
    }
  ; { name = "psi"
    ; latex = "\\psi"
    ; latex_mathp = true
    ; html = "&psi;"
    ; ascii = "psi"
    ; unicode = "ψ"
    }
  ; { name = "tau"
    ; latex = "\\tau"
    ; latex_mathp = true
    ; html = "&tau;"
    ; ascii = "tau"
    ; unicode = "τ"
    }
  ; { name = "Omega"
    ; latex = "\\Omega"
    ; latex_mathp = true
    ; html = "&Omega;"
    ; ascii = "Omega"
    ; unicode = "Ω"
    }
  ; { name = "omega"
    ; latex = "\\omega"
    ; latex_mathp = true
    ; html = "&omega;"
    ; ascii = "omega"
    ; unicode = "ω"
    }
  ; { name = "piv"
    ; latex = "\\varpi"
    ; latex_mathp = true
    ; html = "&piv;"
    ; ascii = "omega-pi"
    ; unicode = "ϖ"
    }
  ; { name = "partial"
    ; latex = "\\partial"
    ; latex_mathp = true
    ; html = "&part;"
    ; ascii = "[partial differential]"
    ; unicode = "∂"
    }
  ; { name = "alefsym"
    ; latex = "\\aleph"
    ; latex_mathp = true
    ; html = "&alefsym;"
    ; ascii = "aleph"
    ; unicode = "ℵ"
    }
  ; { name = "ETH"
    ; latex = "\\DH{}"
    ; latex_mathp = false
    ; html = "&ETH;"
    ; ascii = "D"
    ; unicode = "Ð"
    }
  ; { name = "eth"
    ; latex = "\\dh{}"
    ; latex_mathp = false
    ; html = "&eth;"
    ; ascii = "dh"
    ; unicode = "ð"
    }
  ; { name = "THORN"
    ; latex = "\\TH{}"
    ; latex_mathp = false
    ; html = "&THORN;"
    ; ascii = "TH"
    ; unicode = "Þ"
    }
  ; { name = "thorn"
    ; latex = "\\th{}"
    ; latex_mathp = false
    ; html = "&thorn;"
    ; ascii = "th"
    ; unicode = "þ"
    }
  ; { name = "dots"
    ; latex = "\\dots{}"
    ; latex_mathp = false
    ; html = "&hellip;"
    ; ascii = "..."
    ; unicode = "…"
    }
  ; { name = "hellip"
    ; latex = "\\dots{}"
    ; latex_mathp = false
    ; html = "&hellip;"
    ; ascii = "..."
    ; unicode = "…"
    }
  ; { name = "middot"
    ; latex = "\\textperiodcentered{}"
    ; latex_mathp = false
    ; html = "&middot;"
    ; ascii = "."
    ; unicode = "·"
    }
  ; { name = "iexcl"
    ; latex = "!`"
    ; latex_mathp = false
    ; html = "&iexcl;"
    ; ascii = "!"
    ; unicode = "¡"
    }
  ; { name = "iquest"
    ; latex = "?`"
    ; latex_mathp = false
    ; html = "&iquest;"
    ; ascii = "?"
    ; unicode = "¿"
    }
  ; { name = "shy"
    ; latex = "\\-"
    ; latex_mathp = false
    ; html = "&shy;"
    ; ascii = ""
    ; unicode = ""
    }
  ; { name = "ndash"
    ; latex = "--"
    ; latex_mathp = false
    ; html = "&ndash;"
    ; ascii = "-"
    ; unicode = "–"
    }
  ; { name = "mdash"
    ; latex = "---"
    ; latex_mathp = false
    ; html = "&mdash;"
    ; ascii = "--"
    ; unicode = "—"
    }
  ; { name = "quot"
    ; latex = "\\textquotedbl{}"
    ; latex_mathp = false
    ; html = "&quot;"
    ; ascii = "\""
    ; unicode = "\""
    }
  ; { name = "acute"
    ; latex = "\\textasciiacute{}"
    ; latex_mathp = false
    ; html = "&acute;"
    ; ascii = "'"
    ; unicode = "´"
    }
  ; { name = "ldquo"
    ; latex = "\\textquotedblleft{}"
    ; latex_mathp = false
    ; html = "&ldquo;"
    ; ascii = "\""
    ; unicode = "“"
    }
  ; { name = "rdquo"
    ; latex = "\\textquotedblright{}"
    ; latex_mathp = false
    ; html = "&rdquo;"
    ; ascii = "\""
    ; unicode = "”"
    }
  ; { name = "bdquo"
    ; latex = "\\quotedblbase{}"
    ; latex_mathp = false
    ; html = "&bdquo;"
    ; ascii = "\""
    ; unicode = "„"
    }
  ; { name = "lsquo"
    ; latex = "\\textquoteleft{}"
    ; latex_mathp = false
    ; html = "&lsquo;"
    ; ascii = "`"
    ; unicode = "‘"
    }
  ; { name = "rsquo"
    ; latex = "\\textquoteright{}"
    ; latex_mathp = false
    ; html = "&rsquo;"
    ; ascii = "'"
    ; unicode = "’"
    }
  ; { name = "sbquo"
    ; latex = "\\quotesinglbase{}"
    ; latex_mathp = false
    ; html = "&sbquo;"
    ; ascii = ","
    ; unicode = "‚"
    }
  ; { name = "laquo"
    ; latex = "\\guillemotleft{}"
    ; latex_mathp = false
    ; html = "&laquo;"
    ; ascii = "<<"
    ; unicode = "«"
    }
  ; { name = "raquo"
    ; latex = "\\guillemotright{}"
    ; latex_mathp = false
    ; html = "&raquo;"
    ; ascii = ">>"
    ; unicode = "»"
    }
  ; { name = "lsaquo"
    ; latex = "\\guilsinglleft{}"
    ; latex_mathp = false
    ; html = "&lsaquo;"
    ; ascii = "<"
    ; unicode = "‹"
    }
  ; { name = "rsaquo"
    ; latex = "\\guilsinglright{}"
    ; latex_mathp = false
    ; html = "&rsaquo;"
    ; ascii = ">"
    ; unicode = "›"
    }
  ; { name = "circ"
    ; latex = "\\circ"
    ; latex_mathp = true
    ; html = "&circ;"
    ; ascii = "^"
    ; unicode = "ˆ"
    }
  ; { name = "vert"
    ; latex = "\\vert{}"
    ; latex_mathp = true
    ; html = "&#124;"
    ; ascii = "|"
    ; unicode = "|"
    }
  ; { name = "brvbar"
    ; latex = "\\textbrokenbar{}"
    ; latex_mathp = false
    ; html = "&brvbar;"
    ; ascii = "|"
    ; unicode = "¦"
    }
  ; { name = "sect"
    ; latex = "\\S"
    ; latex_mathp = false
    ; html = "&sect;"
    ; ascii = "paragraph"
    ; unicode = "§"
    }
  ; { name = "amp"
    ; latex = "\\&"
    ; latex_mathp = false
    ; html = "&amp;"
    ; ascii = "&"
    ; unicode = "&"
    }
  ; { name = "lt"
    ; latex = "\\textless{}"
    ; latex_mathp = false
    ; html = "&lt;"
    ; ascii = "<"
    ; unicode = "<"
    }
  ; { name = "gt"
    ; latex = "\\textgreater{}"
    ; latex_mathp = false
    ; html = "&gt;"
    ; ascii = ">"
    ; unicode = ">"
    }
  ; { name = "tilde"
    ; latex = "\\~{}"
    ; latex_mathp = false
    ; html = "&tilde;"
    ; ascii = "~"
    ; unicode = "~"
    }
  ; { name = "dagger"
    ; latex = "\\textdagger{}"
    ; latex_mathp = false
    ; html = "&dagger;"
    ; ascii = "[dagger]"
    ; unicode = "†"
    }
  ; { name = "Dagger"
    ; latex = "\\textdaggerdbl{}"
    ; latex_mathp = false
    ; html = "&Dagger;"
    ; ascii = "[doubledagger]"
    ; unicode = "‡"
    }
  ; { name = "nbsp"
    ; latex = "~"
    ; latex_mathp = false
    ; html = "&nbsp;"
    ; ascii = " "
    ; unicode = " "
    }
  ; { name = "ensp"
    ; latex = "\\hspace*{.5em}"
    ; latex_mathp = false
    ; html = "&ensp;"
    ; ascii = " "
    ; unicode = " "
    }
  ; { name = "emsp"
    ; latex = "\\hspace*{1em}"
    ; latex_mathp = false
    ; html = "&emsp;"
    ; ascii = " "
    ; unicode = " "
    }
  ; { name = "thinsp"
    ; latex = "\\hspace*{.2em}"
    ; latex_mathp = false
    ; html = "&thinsp;"
    ; ascii = " "
    ; unicode = " "
    }
  ; { name = "curren"
    ; latex = "\\textcurrency{}"
    ; latex_mathp = false
    ; html = "&curren;"
    ; ascii = "curr."
    ; unicode = "¤"
    }
  ; { name = "cent"
    ; latex = "\\textcent{}"
    ; latex_mathp = false
    ; html = "&cent;"
    ; ascii = "cent"
    ; unicode = "¢"
    }
  ; { name = "pound"
    ; latex = "\\pounds{}"
    ; latex_mathp = false
    ; html = "&pound;"
    ; ascii = "pound"
    ; unicode = "£"
    }
  ; { name = "yen"
    ; latex = "\\textyen{}"
    ; latex_mathp = false
    ; html = "&yen;"
    ; ascii = "yen"
    ; unicode = "¥"
    }
  ; { name = "euro"
    ; latex = "\\texteuro{}"
    ; latex_mathp = false
    ; html = "&euro;"
    ; ascii = "EUR"
    ; unicode = "€"
    }
  ; { name = "EUR"
    ; latex = "\\EUR{}"
    ; latex_mathp = false
    ; html = "&euro;"
    ; ascii = "EUR"
    ; unicode = "€"
    }
  ; { name = "EURdig"
    ; latex = "\\EURdig{}"
    ; latex_mathp = false
    ; html = "&euro;"
    ; ascii = "EUR"
    ; unicode = "€"
    }
  ; { name = "EURhv"
    ; latex = "\\EURhv{}"
    ; latex_mathp = false
    ; html = "&euro;"
    ; ascii = "EUR"
    ; unicode = "€"
    }
  ; { name = "EURcr"
    ; latex = "\\EURcr{}"
    ; latex_mathp = false
    ; html = "&euro;"
    ; ascii = "EUR"
    ; unicode = "€"
    }
  ; { name = "EURtm"
    ; latex = "\\EURtm{}"
    ; latex_mathp = false
    ; html = "&euro;"
    ; ascii = "EUR"
    ; unicode = "€"
    }
  ; { name = "copy"
    ; latex = "\\textcopyright{}"
    ; latex_mathp = false
    ; html = "&copy;"
    ; ascii = "(c)"
    ; unicode = "©"
    }
  ; { name = "reg"
    ; latex = "\\textregistered{}"
    ; latex_mathp = false
    ; html = "&reg;"
    ; ascii = "(r)"
    ; unicode = "®"
    }
  ; { name = "trade"
    ; latex = "\\texttrademark{}"
    ; latex_mathp = false
    ; html = "&trade;"
    ; ascii = "TM"
    ; unicode = "™"
    }
  ; { name = "minus"
    ; latex = "\\minus"
    ; latex_mathp = true
    ; html = "&minus;"
    ; ascii = "-"
    ; unicode = "−"
    }
  ; { name = "pm"
    ; latex = "\\textpm{}"
    ; latex_mathp = false
    ; html = "&plusmn;"
    ; ascii = "+-"
    ; unicode = "±"
    }
  ; { name = "plusmn"
    ; latex = "\\textpm{}"
    ; latex_mathp = false
    ; html = "&plusmn;"
    ; ascii = "+-"
    ; unicode = "±"
    }
  ; { name = "times"
    ; latex = "\\texttimes{}"
    ; latex_mathp = false
    ; html = "&times;"
    ; ascii = "*"
    ; unicode = "×"
    }
  ; { name = "frasl"
    ; latex = "/"
    ; latex_mathp = false
    ; html = "&frasl;"
    ; ascii = "/"
    ; unicode = "⁄"
    }
  ; { name = "div"
    ; latex = "\\textdiv{}"
    ; latex_mathp = false
    ; html = "&divide;"
    ; ascii = "/"
    ; unicode = "÷"
    }
  ; { name = "frac12"
    ; latex = "\\textonehalf{}"
    ; latex_mathp = false
    ; html = "&frac12;"
    ; ascii = "1/2"
    ; unicode = "½"
    }
  ; { name = "frac14"
    ; latex = "\\textonequarter{}"
    ; latex_mathp = false
    ; html = "&frac14;"
    ; ascii = "1/4"
    ; unicode = "¼"
    }
  ; { name = "frac34"
    ; latex = "\\textthreequarters{}"
    ; latex_mathp = false
    ; html = "&frac34;"
    ; ascii = "3/4"
    ; unicode = "¾"
    }
  ; { name = "permil"
    ; latex = "\\textperthousand{}"
    ; latex_mathp = false
    ; html = "&permil;"
    ; ascii = "per thousand"
    ; unicode = "‰"
    }
  ; { name = "sup1"
    ; latex = "\\textonesuperior{}"
    ; latex_mathp = false
    ; html = "&sup1;"
    ; ascii = "^1"
    ; unicode = "¹"
    }
  ; { name = "sup2"
    ; latex = "\\texttwosuperior{}"
    ; latex_mathp = false
    ; html = "&sup2;"
    ; ascii = "^2"
    ; unicode = "²"
    }
  ; { name = "sup3"
    ; latex = "\\textthreesuperior{}"
    ; latex_mathp = false
    ; html = "&sup3;"
    ; ascii = "^3"
    ; unicode = "³"
    }
  ; { name = "radic"
    ; latex = "\\sqrt{\\,}"
    ; latex_mathp = true
    ; html = "&radic;"
    ; ascii = "[square root]"
    ; unicode = "√"
    }
  ; { name = "sum"
    ; latex = "\\sum"
    ; latex_mathp = true
    ; html = "&sum;"
    ; ascii = "[sum]"
    ; unicode = "∑"
    }
  ; { name = "prod"
    ; latex = "\\prod"
    ; latex_mathp = true
    ; html = "&prod;"
    ; ascii = "[product]"
    ; unicode = "∏"
    }
  ; { name = "micro"
    ; latex = "\\textmu{}"
    ; latex_mathp = false
    ; html = "&micro;"
    ; ascii = "micro"
    ; unicode = "µ"
    }
  ; { name = "macr"
    ; latex = "\\textasciimacron{}"
    ; latex_mathp = false
    ; html = "&macr;"
    ; ascii = "[macron]"
    ; unicode = "¯"
    }
  ; { name = "deg"
    ; latex = "\\textdegree{}"
    ; latex_mathp = false
    ; html = "&deg;"
    ; ascii = "degree"
    ; unicode = "°"
    }
  ; { name = "prime"
    ; latex = "\\prime"
    ; latex_mathp = true
    ; html = "&prime;"
    ; ascii = "'"
    ; unicode = "′"
    }
  ; { name = "Prime"
    ; latex = "\\prime{}\\prime"
    ; latex_mathp = true
    ; html = "&Prime;"
    ; ascii = "''"
    ; unicode = "″"
    }
  ; { name = "infin"
    ; latex = "\\propto"
    ; latex_mathp = true
    ; html = "&infin;"
    ; ascii = "[infinity]"
    ; unicode = "∞"
    }
  ; { name = "infty"
    ; latex = "\\infty"
    ; latex_mathp = true
    ; html = "&infin;"
    ; ascii = "[infinity]"
    ; unicode = "∞"
    }
  ; { name = "prop"
    ; latex = "\\propto"
    ; latex_mathp = true
    ; html = "&prop;"
    ; ascii = "[proportional to]"
    ; unicode = "∝"
    }
  ; { name = "proptp"
    ; latex = "\\propto"
    ; latex_mathp = true
    ; html = "&prop;"
    ; ascii = "[proportional to]"
    ; unicode = "∝"
    }
  ; { name = "not"
    ; latex = "\\textlnot{}"
    ; latex_mathp = false
    ; html = "&not;"
    ; ascii = "[angled dash]"
    ; unicode = "¬"
    }
  ; { name = "land"
    ; latex = "\\land"
    ; latex_mathp = true
    ; html = "&and;"
    ; ascii = "[logical and]"
    ; unicode = "∧"
    }
  ; { name = "wedge"
    ; latex = "\\wedge"
    ; latex_mathp = true
    ; html = "&and;"
    ; ascii = "[logical and]"
    ; unicode = "∧"
    }
  ; { name = "lor"
    ; latex = "\\lor"
    ; latex_mathp = true
    ; html = "&or;"
    ; ascii = "[logical or]"
    ; unicode = "∨"
    }
  ; { name = "vee"
    ; latex = "\\vee"
    ; latex_mathp = true
    ; html = "&or;"
    ; ascii = "[logical or]"
    ; unicode = "∨"
    }
  ; { name = "cap"
    ; latex = "\\cap"
    ; latex_mathp = true
    ; html = "&cap;"
    ; ascii = "[intersection]"
    ; unicode = "∩"
    }
  ; { name = "cup"
    ; latex = "\\cup"
    ; latex_mathp = true
    ; html = "&cup;"
    ; ascii = "[union]"
    ; unicode = "∪"
    }
  ; { name = "int"
    ; latex = "\\int"
    ; latex_mathp = true
    ; html = "&int;"
    ; ascii = "[integral]"
    ; unicode = "∫"
    }
  ; { name = "there4"
    ; latex = "\\therefore"
    ; latex_mathp = true
    ; html = "&there4;"
    ; ascii = "[therefore]"
    ; unicode = "∴"
    }
  ; { name = "sim"
    ; latex = "\\sim"
    ; latex_mathp = true
    ; html = "&sim;"
    ; ascii = "~"
    ; unicode = "∼"
    }
  ; { name = "cong"
    ; latex = "\\cong"
    ; latex_mathp = true
    ; html = "&cong;"
    ; ascii = "[approx. equal to]"
    ; unicode = "≅"
    }
  ; { name = "simeq"
    ; latex = "\\simeq"
    ; latex_mathp = true
    ; html = "&cong;"
    ; ascii = "[approx. equal to]"
    ; unicode = "≅"
    }
  ; { name = "asymp"
    ; latex = "\\asymp"
    ; latex_mathp = true
    ; html = "&asymp;"
    ; ascii = "[almost equal to]"
    ; unicode = "≈"
    }
  ; { name = "approx"
    ; latex = "\\approx"
    ; latex_mathp = true
    ; html = "&asymp;"
    ; ascii = "[almost equal to]"
    ; unicode = "≈"
    }
  ; { name = "ne"
    ; latex = "\\ne"
    ; latex_mathp = true
    ; html = "&ne;"
    ; ascii = "[not equal to]"
    ; unicode = "≠"
    }
  ; { name = "neq"
    ; latex = "\\neq"
    ; latex_mathp = true
    ; html = "&ne;"
    ; ascii = "[not equal to]"
    ; unicode = "≠"
    }
  ; { name = "equiv"
    ; latex = "\\equiv"
    ; latex_mathp = true
    ; html = "&equiv;"
    ; ascii = "[identical to]"
    ; unicode = "≡"
    }
  ; { name = "le"
    ; latex = "\\le"
    ; latex_mathp = true
    ; html = "&le;"
    ; ascii = "<="
    ; unicode = "≤"
    }
  ; { name = "ge"
    ; latex = "\\ge"
    ; latex_mathp = true
    ; html = "&ge;"
    ; ascii = ">="
    ; unicode = "≥"
    }
  ; { name = "sub"
    ; latex = "\\subset"
    ; latex_mathp = true
    ; html = "&sub;"
    ; ascii = "[subset of]"
    ; unicode = "⊂"
    }
  ; { name = "subset"
    ; latex = "\\subset"
    ; latex_mathp = true
    ; html = "&sub;"
    ; ascii = "[subset of]"
    ; unicode = "⊂"
    }
  ; { name = "sup"
    ; latex = "\\supset"
    ; latex_mathp = true
    ; html = "&sup;"
    ; ascii = "[superset of]"
    ; unicode = "⊃"
    }
  ; { name = "supset"
    ; latex = "\\supset"
    ; latex_mathp = true
    ; html = "&sup;"
    ; ascii = "[superset of]"
    ; unicode = "⊃"
    }
  ; { name = "nsub"
    ; latex = "\\not\\subset"
    ; latex_mathp = true
    ; html = "&nsub;"
    ; ascii = "[not a subset of]"
    ; unicode = "⊄"
    }
  ; { name = "sube"
    ; latex = "\\subseteq"
    ; latex_mathp = true
    ; html = "&sube;"
    ; ascii = "[subset of or equal to]"
    ; unicode = "⊆"
    }
  ; { name = "nsup"
    ; latex = "\\not\\supset"
    ; latex_mathp = true
    ; html = "&nsup;"
    ; ascii = "[not a superset of]"
    ; unicode = "⊅"
    }
  ; { name = "supe"
    ; latex = "\\supseteq"
    ; latex_mathp = true
    ; html = "&supe;"
    ; ascii = "[superset of or equal to]"
    ; unicode = "⊇"
    }
  ; { name = "forall"
    ; latex = "\\forall"
    ; latex_mathp = true
    ; html = "&forall;"
    ; ascii = "[for all]"
    ; unicode = "∀"
    }
  ; { name = "exist"
    ; latex = "\\exists"
    ; latex_mathp = true
    ; html = "&exist;"
    ; ascii = "[there exists]"
    ; unicode = "∃"
    }
  ; { name = "exists"
    ; latex = "\\exists"
    ; latex_mathp = true
    ; html = "&exist;"
    ; ascii = "[there exists]"
    ; unicode = "∃"
    }
  ; { name = "empty"
    ; latex = "\\empty"
    ; latex_mathp = true
    ; html = "&empty;"
    ; ascii = "[empty set]"
    ; unicode = "∅"
    }
  ; { name = "emptyset"
    ; latex = "\\emptyset"
    ; latex_mathp = true
    ; html = "&empty;"
    ; ascii = "[empty set]"
    ; unicode = "∅"
    }
  ; { name = "isin"
    ; latex = "\\in"
    ; latex_mathp = true
    ; html = "&isin;"
    ; ascii = "[element of]"
    ; unicode = "∈"
    }
  ; { name = "in"
    ; latex = "\\in"
    ; latex_mathp = true
    ; html = "&isin;"
    ; ascii = "[element of]"
    ; unicode = "∈"
    }
  ; { name = "notin"
    ; latex = "\\notin"
    ; latex_mathp = true
    ; html = "&notin;"
    ; ascii = "[not an element of]"
    ; unicode = "∉"
    }
  ; { name = "ni"
    ; latex = "\\ni"
    ; latex_mathp = true
    ; html = "&ni;"
    ; ascii = "[contains as member]"
    ; unicode = "∋"
    }
  ; { name = "nabla"
    ; latex = "\\nabla"
    ; latex_mathp = true
    ; html = "&nabla;"
    ; ascii = "[nabla]"
    ; unicode = "∇"
    }
  ; { name = "ang"
    ; latex = "\\angle"
    ; latex_mathp = true
    ; html = "&ang;"
    ; ascii = "[angle]"
    ; unicode = "∠"
    }
  ; { name = "angle"
    ; latex = "\\angle"
    ; latex_mathp = true
    ; html = "&ang;"
    ; ascii = "[angle]"
    ; unicode = "∠"
    }
  ; { name = "perp"
    ; latex = "\\perp"
    ; latex_mathp = true
    ; html = "&perp;"
    ; ascii = "[up tack]"
    ; unicode = "⊥"
    }
  ; { name = "sdot"
    ; latex = "\\cdot"
    ; latex_mathp = true
    ; html = "&sdot;"
    ; ascii = "[dot]"
    ; unicode = "⋅"
    }
  ; { name = "cdot"
    ; latex = "\\cdot"
    ; latex_mathp = true
    ; html = "&sdot;"
    ; ascii = "[dot]"
    ; unicode = "⋅"
    }
  ; { name = "lceil"
    ; latex = "\\lceil"
    ; latex_mathp = true
    ; html = "&lceil;"
    ; ascii = "[left ceiling]"
    ; unicode = "⌈"
    }
  ; { name = "rceil"
    ; latex = "\\rceil"
    ; latex_mathp = true
    ; html = "&rceil;"
    ; ascii = "[right ceiling]"
    ; unicode = "⌉"
    }
  ; { name = "lfloor"
    ; latex = "\\lfloor"
    ; latex_mathp = true
    ; html = "&lfloor;"
    ; ascii = "[left floor]"
    ; unicode = "⌊"
    }
  ; { name = "rfloor"
    ; latex = "\\rfloor"
    ; latex_mathp = true
    ; html = "&rfloor;"
    ; ascii = "[right floor]"
    ; unicode = "⌋"
    }
  ; { name = "lang"
    ; latex = "\\langle"
    ; latex_mathp = true
    ; html = "&lang;"
    ; ascii = "<"
    ; unicode = "⟨"
    }
  ; { name = "rang"
    ; latex = "\\rangle"
    ; latex_mathp = true
    ; html = "&rang;"
    ; ascii = ">"
    ; unicode = "⟩"
    }
  ; { name = "larr"
    ; latex = "\\leftarrow"
    ; latex_mathp = true
    ; html = "&larr;"
    ; ascii = "<-"
    ; unicode = "←"
    }
  ; { name = "leftarrow"
    ; latex = "\\leftarrow"
    ; latex_mathp = true
    ; html = "&larr;"
    ; ascii = "<-"
    ; unicode = "←"
    }
  ; { name = "gets"
    ; latex = "\\gets"
    ; latex_mathp = true
    ; html = "&larr;"
    ; ascii = "<-"
    ; unicode = "←"
    }
  ; { name = "lArr"
    ; latex = "\\Leftarrow"
    ; latex_mathp = true
    ; html = "&lArr;"
    ; ascii = "<="
    ; unicode = "⇐"
    }
  ; { name = "Leftarrow"
    ; latex = "\\Leftarrow"
    ; latex_mathp = true
    ; html = "&lArr;"
    ; ascii = "<="
    ; unicode = "⇐"
    }
  ; { name = "uarr"
    ; latex = "\\uparrow"
    ; latex_mathp = true
    ; html = "&uarr;"
    ; ascii = "[uparrow]"
    ; unicode = "↑"
    }
  ; { name = "uparrow"
    ; latex = "\\uparrow"
    ; latex_mathp = true
    ; html = "&uarr;"
    ; ascii = "[uparrow]"
    ; unicode = "↑"
    }
  ; { name = "uArr"
    ; latex = "\\Uparrow"
    ; latex_mathp = true
    ; html = "&uArr;"
    ; ascii = "[dbluparrow]"
    ; unicode = "⇑"
    }
  ; { name = "Uparrow"
    ; latex = "\\Uparrow"
    ; latex_mathp = true
    ; html = "&uArr;"
    ; ascii = "[dbluparrow]"
    ; unicode = "⇑"
    }
  ; { name = "rarr"
    ; latex = "\\rightarrow"
    ; latex_mathp = true
    ; html = "&rarr;"
    ; ascii = "->"
    ; unicode = "→"
    }
  ; { name = "to"
    ; latex = "\\to"
    ; latex_mathp = true
    ; html = "&rarr;"
    ; ascii = "->"
    ; unicode = "→"
    }
  ; { name = "rightarrow"
    ; latex = "\\rightarrow"
    ; latex_mathp = true
    ; html = "&rarr;"
    ; ascii = "->"
    ; unicode = "→"
    }
  ; { name = "rArr"
    ; latex = "\\Rightarrow"
    ; latex_mathp = true
    ; html = "&rArr;"
    ; ascii = "=>"
    ; unicode = "⇒"
    }
  ; { name = "Rightarrow"
    ; latex = "\\Rightarrow"
    ; latex_mathp = true
    ; html = "&rArr;"
    ; ascii = "=>"
    ; unicode = "⇒"
    }
  ; { name = "darr"
    ; latex = "\\downarrow"
    ; latex_mathp = true
    ; html = "&darr;"
    ; ascii = "[downarrow]"
    ; unicode = "↓"
    }
  ; { name = "downarrow"
    ; latex = "\\downarrow"
    ; latex_mathp = true
    ; html = "&darr;"
    ; ascii = "[downarrow]"
    ; unicode = "↓"
    }
  ; { name = "dArr"
    ; latex = "\\Downarrow"
    ; latex_mathp = true
    ; html = "&dArr;"
    ; ascii = "[dbldownarrow]"
    ; unicode = "⇓"
    }
  ; { name = "Downarrow"
    ; latex = "\\Downarrow"
    ; latex_mathp = true
    ; html = "&dArr;"
    ; ascii = "[dbldownarrow]"
    ; unicode = "⇓"
    }
  ; { name = "harr"
    ; latex = "\\leftrightarrow"
    ; latex_mathp = true
    ; html = "&harr;"
    ; ascii = "<->"
    ; unicode = "↔"
    }
  ; { name = "leftrightarrow"
    ; latex = "\\leftrightarrow"
    ; latex_mathp = true
    ; html = "&harr;"
    ; ascii = "<->"
    ; unicode = "↔"
    }
  ; { name = "hArr"
    ; latex = "\\Leftrightarrow"
    ; latex_mathp = true
    ; html = "&hArr;"
    ; ascii = "<=>"
    ; unicode = "⇔"
    }
  ; { name = "Leftrightarrow"
    ; latex = "\\Leftrightarrow"
    ; latex_mathp = true
    ; html = "&hArr;"
    ; ascii = "<=>"
    ; unicode = "⇔"
    }
  ; { name = "crarr"
    ; latex = "\\hookleftarrow"
    ; latex_mathp = true
    ; html = "&crarr;"
    ; ascii = "<-'"
    ; unicode = "↵"
    }
  ; { name = "hookleftarrow"
    ; latex = "\\hookleftarrow"
    ; latex_mathp = true
    ; html = "&crarr;"
    ; ascii = "<-'"
    ; unicode = "↵"
    }
  ; { name = "arccos"
    ; latex = "\\arccos"
    ; latex_mathp = true
    ; html = "arccos"
    ; ascii = "arccos"
    ; unicode = "arccos"
    }
  ; { name = "arcsin"
    ; latex = "\\arcsin"
    ; latex_mathp = true
    ; html = "arcsin"
    ; ascii = "arcsin"
    ; unicode = "arcsin"
    }
  ; { name = "arctan"
    ; latex = "\\arctan"
    ; latex_mathp = true
    ; html = "arctan"
    ; ascii = "arctan"
    ; unicode = "arctan"
    }
  ; { name = "arg"
    ; latex = "\\arg"
    ; latex_mathp = true
    ; html = "arg"
    ; ascii = "arg"
    ; unicode = "arg"
    }
  ; { name = "cos"
    ; latex = "\\cos"
    ; latex_mathp = true
    ; html = "cos"
    ; ascii = "cos"
    ; unicode = "cos"
    }
  ; { name = "cosh"
    ; latex = "\\cosh"
    ; latex_mathp = true
    ; html = "cosh"
    ; ascii = "cosh"
    ; unicode = "cosh"
    }
  ; { name = "cot"
    ; latex = "\\cot"
    ; latex_mathp = true
    ; html = "cot"
    ; ascii = "cot"
    ; unicode = "cot"
    }
  ; { name = "coth"
    ; latex = "\\coth"
    ; latex_mathp = true
    ; html = "coth"
    ; ascii = "coth"
    ; unicode = "coth"
    }
  ; { name = "csc"
    ; latex = "\\csc"
    ; latex_mathp = true
    ; html = "csc"
    ; ascii = "csc"
    ; unicode = "csc"
    }
  ; { name = "deg"
    ; latex = "\\deg"
    ; latex_mathp = true
    ; html = "&deg;"
    ; ascii = "deg"
    ; unicode = "deg"
    }
  ; { name = "det"
    ; latex = "\\det"
    ; latex_mathp = true
    ; html = "det"
    ; ascii = "det"
    ; unicode = "det"
    }
  ; { name = "dim"
    ; latex = "\\dim"
    ; latex_mathp = true
    ; html = "dim"
    ; ascii = "dim"
    ; unicode = "dim"
    }
  ; { name = "exp"
    ; latex = "\\exp"
    ; latex_mathp = true
    ; html = "exp"
    ; ascii = "exp"
    ; unicode = "exp"
    }
  ; { name = "gcd"
    ; latex = "\\gcd"
    ; latex_mathp = true
    ; html = "gcd"
    ; ascii = "gcd"
    ; unicode = "gcd"
    }
  ; { name = "hom"
    ; latex = "\\hom"
    ; latex_mathp = true
    ; html = "hom"
    ; ascii = "hom"
    ; unicode = "hom"
    }
  ; { name = "inf"
    ; latex = "\\inf"
    ; latex_mathp = true
    ; html = "inf"
    ; ascii = "inf"
    ; unicode = "inf"
    }
  ; { name = "ker"
    ; latex = "\\ker"
    ; latex_mathp = true
    ; html = "ker"
    ; ascii = "ker"
    ; unicode = "ker"
    }
  ; { name = "lg"
    ; latex = "\\lg"
    ; latex_mathp = true
    ; html = "lg"
    ; ascii = "lg"
    ; unicode = "lg"
    }
  ; { name = "lim"
    ; latex = "\\lim"
    ; latex_mathp = true
    ; html = "lim"
    ; ascii = "lim"
    ; unicode = "lim"
    }
  ; { name = "liminf"
    ; latex = "\\liminf"
    ; latex_mathp = true
    ; html = "liminf"
    ; ascii = "liminf"
    ; unicode = "liminf"
    }
  ; { name = "limsup"
    ; latex = "\\limsup"
    ; latex_mathp = true
    ; html = "limsup"
    ; ascii = "limsup"
    ; unicode = "limsup"
    }
  ; { name = "ln"
    ; latex = "\\ln"
    ; latex_mathp = true
    ; html = "ln"
    ; ascii = "ln"
    ; unicode = "ln"
    }
  ; { name = "log"
    ; latex = "\\log"
    ; latex_mathp = true
    ; html = "log"
    ; ascii = "log"
    ; unicode = "log"
    }
  ; { name = "max"
    ; latex = "\\max"
    ; latex_mathp = true
    ; html = "max"
    ; ascii = "max"
    ; unicode = "max"
    }
  ; { name = "min"
    ; latex = "\\min"
    ; latex_mathp = true
    ; html = "min"
    ; ascii = "min"
    ; unicode = "min"
    }
  ; { name = "Pr"
    ; latex = "\\Pr"
    ; latex_mathp = true
    ; html = "Pr"
    ; ascii = "Pr"
    ; unicode = "Pr"
    }
  ; { name = "sec"
    ; latex = "\\sec"
    ; latex_mathp = true
    ; html = "sec"
    ; ascii = "sec"
    ; unicode = "sec"
    }
  ; { name = "sin"
    ; latex = "\\sin"
    ; latex_mathp = true
    ; html = "sin"
    ; ascii = "sin"
    ; unicode = "sin"
    }
  ; { name = "sinh"
    ; latex = "\\sinh"
    ; latex_mathp = true
    ; html = "sinh"
    ; ascii = "sinh"
    ; unicode = "sinh"
    }
  ; { name = "sup"
    ; latex = "\\sup"
    ; latex_mathp = true
    ; html = "&sup;"
    ; ascii = "sup"
    ; unicode = "sup"
    }
  ; { name = "tan"
    ; latex = "\\tan"
    ; latex_mathp = true
    ; html = "tan"
    ; ascii = "tan"
    ; unicode = "tan"
    }
  ; { name = "tanh"
    ; latex = "\\tanh"
    ; latex_mathp = true
    ; html = "tanh"
    ; ascii = "tanh"
    ; unicode = "tanh"
    }
  ; { name = "bull"
    ; latex = "\\textbullet{}"
    ; latex_mathp = false
    ; html = "&bull;"
    ; ascii = "*"
    ; unicode = "•"
    }
  ; { name = "bullet"
    ; latex = "\\textbullet{}"
    ; latex_mathp = false
    ; html = "&bull;"
    ; ascii = "*"
    ; unicode = "•"
    }
  ; { name = "star"
    ; latex = "\\star"
    ; latex_mathp = true
    ; html = "*"
    ; ascii = "*"
    ; unicode = "⋆"
    }
  ; { name = "lowast"
    ; latex = "\\ast"
    ; latex_mathp = true
    ; html = "&lowast;"
    ; ascii = "*"
    ; unicode = "∗"
    }
  ; { name = "ast"
    ; latex = "\\ast"
    ; latex_mathp = true
    ; html = "&lowast;"
    ; ascii = "*"
    ; unicode = "*"
    }
  ; { name = "odot"
    ; latex = "\\odot"
    ; latex_mathp = true
    ; html = "o"
    ; ascii = "[circled dot]"
    ; unicode = "ʘ"
    }
  ; { name = "oplus"
    ; latex = "\\oplus"
    ; latex_mathp = true
    ; html = "&oplus;"
    ; ascii = "[circled plus]"
    ; unicode = "⊕"
    }
  ; { name = "otimes"
    ; latex = "\\otimes"
    ; latex_mathp = true
    ; html = "&otimes;"
    ; ascii = "[circled times]"
    ; unicode = "⊗"
    }
  ; { name = "checkmark"
    ; latex = "\\checkmark"
    ; latex_mathp = true
    ; html = "&#10003;"
    ; ascii = "[checkmark]"
    ; unicode = "✓"
    }
  ; { name = "para"
    ; latex = "\\P{}"
    ; latex_mathp = false
    ; html = "&para;"
    ; ascii = "[pilcrow]"
    ; unicode = "¶"
    }
  ; { name = "ordf"
    ; latex = "\\textordfeminine{}"
    ; latex_mathp = false
    ; html = "&ordf;"
    ; ascii = "_a_"
    ; unicode = "ª"
    }
  ; { name = "ordm"
    ; latex = "\\textordmasculine{}"
    ; latex_mathp = false
    ; html = "&ordm;"
    ; ascii = "_o_"
    ; unicode = "º"
    }
  ; { name = "cedil"
    ; latex = "\\c{}"
    ; latex_mathp = false
    ; html = "&cedil;"
    ; ascii = "[cedilla]"
    ; unicode = "¸"
    }
  ; { name = "oline"
    ; latex = "\\overline{~}"
    ; latex_mathp = true
    ; html = "&oline;"
    ; ascii = "[overline]"
    ; unicode = "‾"
    }
  ; { name = "uml"
    ; latex = "\\textasciidieresis{}"
    ; latex_mathp = false
    ; html = "&uml;"
    ; ascii = "[diaeresis]"
    ; unicode = "¨"
    }
  ; { name = "zwnj"
    ; latex = "\\/{}"
    ; latex_mathp = false
    ; html = "&zwnj;"
    ; ascii = ""
    ; unicode = "‌"
    }
  ; { name = "zwj"
    ; latex = ""
    ; latex_mathp = false
    ; html = "&zwj;"
    ; ascii = ""
    ; unicode = "‍"
    }
  ; { name = "lrm"
    ; latex = ""
    ; latex_mathp = false
    ; html = "&lrm;"
    ; ascii = ""
    ; unicode = "‎"
    }
  ; { name = "rlm"
    ; latex = ""
    ; latex_mathp = false
    ; html = "&rlm;"
    ; ascii = ""
    ; unicode = "‏"
    }
  ; { name = "smile"
    ; latex = "\\smile"
    ; latex_mathp = true
    ; html = "&#9786;"
    ; ascii = ":-)"
    ; unicode = "⌣"
    }
  ; { name = "smiley"
    ; latex = "\\smiley{}"
    ; latex_mathp = false
    ; html = "&#9786;"
    ; ascii = ":-)"
    ; unicode = "☺"
    }
  ; { name = "blacksmile"
    ; latex = "\\blacksmiley{}"
    ; latex_mathp = false
    ; html = "&#9787;"
    ; ascii = ":-)"
    ; unicode = "☻"
    }
  ; { name = "sad"
    ; latex = "\\frownie{}"
    ; latex_mathp = false
    ; html = "&#9785;"
    ; ascii = ":-("
    ; unicode = "☹"
    }
  ; { name = "clubs"
    ; latex = "\\clubsuit"
    ; latex_mathp = true
    ; html = "&clubs;"
    ; ascii = "[clubs]"
    ; unicode = "♣"
    }
  ; { name = "clubsuit"
    ; latex = "\\clubsuit"
    ; latex_mathp = true
    ; html = "&clubs;"
    ; ascii = "[clubs]"
    ; unicode = "♣"
    }
  ; { name = "spades"
    ; latex = "\\spadesuit"
    ; latex_mathp = true
    ; html = "&spades;"
    ; ascii = "[spades]"
    ; unicode = "♠"
    }
  ; { name = "spadesuit"
    ; latex = "\\spadesuit"
    ; latex_mathp = true
    ; html = "&spades;"
    ; ascii = "[spades]"
    ; unicode = "♠"
    }
  ; { name = "hearts"
    ; latex = "\\heartsuit"
    ; latex_mathp = true
    ; html = "&hearts;"
    ; ascii = "[hearts]"
    ; unicode = "♥"
    }
  ; { name = "heartsuit"
    ; latex = "\\heartsuit"
    ; latex_mathp = true
    ; html = "&heartsuit;"
    ; ascii = "[hearts]"
    ; unicode = "♥"
    }
  ; { name = "diams"
    ; latex = "\\diamondsuit"
    ; latex_mathp = true
    ; html = "&diams;"
    ; ascii = "[diamonds]"
    ; unicode = "♦"
    }
  ; { name = "diamondsuit"
    ; latex = "\\diamondsuit"
    ; latex_mathp = true
    ; html = "&diams;"
    ; ascii = "[diamonds]"
    ; unicode = "♦"
    }
  ; { name = "Diamond"
    ; latex = "\\diamond"
    ; latex_mathp = true
    ; html = "&diamond;"
    ; ascii = "[diamond]"
    ; unicode = "⋄"
    }
  ; { name = "loz"
    ; latex = "\\diamond"
    ; latex_mathp = true
    ; html = "&loz;"
    ; ascii = "[lozenge]"
    ; unicode = "◊"
    }
  ]

let data_tbl =
  let tbl = Hashtbl.create 1000 in
  let _ = List.iter (fun entity -> Hashtbl.add tbl entity.name entity) data in
  tbl

(*
The source code to generate that is (+ a replacement on t/nil)
: (mapcar (lambda (x) (if (not (stringp x)) (insert (format "{ name = %S; latex = %S; latex_mathp = %S; html = %S; ascii = %S; unicode = %S };\n" (car x) (cadr x) (caddr x) (cadddr x) (cadddr (cdr x)) (cadddr (cdddr x)))))) org-entities)
*)

let find name = Hashtbl.find data_tbl name

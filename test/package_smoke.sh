#!/bin/sh
set -eu

# Exercise installed artifacts, outside the source/build tree.
package=${1:?usage: package_smoke.sh PACKAGE PREFIX}
prefix=${2:?usage: package_smoke.sh PACKAGE PREFIX}
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT HUP INT TERM

case "$package" in
  mldoc)
    test -f "$prefix/lib/mldoc/META"
    test ! -e "$prefix/bin/mldoc"
    test ! -e "$prefix/bin/mldoc-bench"
    test ! -e "$prefix/share/mldoc-js/index.js"
    cat > "$work/dune-project" <<'EOF'
(lang dune 3.5)
EOF
    cat > "$work/dune" <<'EOF'
(executable (name probe) (libraries mldoc))
EOF
    cat > "$work/probe.ml" <<'EOF'
let config format outline =
  match Mldoc.Conf.of_yojson
          (`Assoc [ "format", `String format;
                    "keep_line_break", `Bool false;
                    "parse_outline_only", `Bool outline ]) with
  | Ok c -> c
  | Error e -> failwith e

let check format outline input =
  let ast = Mldoc.Parser.parse (config format outline) input in
  if input <> "" && ast = [] then failwith "empty parse";
  match Mldoc.Type.blocks_of_yojson (Mldoc.Type.blocks_to_yojson ast) with
  | Ok decoded when decoded = ast -> ()
  | _ -> failwith "AST JSON round trip"

let () =
  List.iter (fun outline ->
    List.iter (check "Markdown" outline)
      [ ""; "- Package smoke λ with [[page]]\r\n  id:: package-smoke\r\n" ];
    List.iter (check "Org" outline)
      [ ""; "* Package smoke λ\r\nOrg paragraph\r\n" ]) [false; true];
  print_endline "installed library: OK"
EOF
    OCAMLPATH="$prefix/lib${OCAMLPATH:+:$OCAMLPATH}" \
      dune exec --root "$work" ./probe.exe
    ;;
  mldoc-cli)
    command_path="$prefix/bin/mldoc"
    test -x "$command_path"
    # The existing native CLI selects Org for stdin, so compare an Org file.
    printf '%s\r\n' '* Package smoke λ' 'second line' > "$work/input.org"
    "$command_path" "$work/input.org" -b markdown -o - > "$work/file.out"
    "$command_path" - -b markdown -o - < "$work/input.org" > "$work/stdin.out"
    cmp "$work/file.out" "$work/stdin.out"
    test -s "$work/file.out"
    "$command_path" "$work/input.org" -b markdown -o "$work/output.md"
    cmp "$work/file.out" "$work/output.md"
    "$command_path" - -b markdown -o - < /dev/null > "$work/empty.out"
    if "$command_path" --not-a-mldoc-option > "$work/error.out" 2>&1; then
      echo 'invalid CLI option unexpectedly succeeded' >&2
      exit 1
    fi
    echo 'installed CLI: OK'
    ;;
  mldoc-js)
    node "$script_dir/package_js_smoke.js" "$prefix/share/mldoc-js/index.js"
    ;;
  mldoc-bench)
    mkdir "$work/examples" "$work/graph" "$work/graph/pages"
    cp "$prefix/share/mldoc-bench/examples/doc.org" "$work/examples/doc.org"
    cp "$prefix/share/mldoc-bench/examples/syntax.md" "$work/examples/syntax.md"
    printf '%s\n' '- Synthetic package fixture' > "$work/graph/pages/page.md"
    (
      cd "$work"
      "$prefix/bin/mldoc-bench" -help > help.out
      "$prefix/bin/mldoc-time-parse" > parse.out
      "$prefix/bin/mldoc-time-graph" "$work/graph" > graph.out
      test -s help.out
      test -s parse.out
      test -s graph.out
    )
    echo 'installed benchmarks (synthetic fixtures): OK'
    ;;
  *)
    echo "unknown package: $package" >&2
    exit 2
    ;;
esac

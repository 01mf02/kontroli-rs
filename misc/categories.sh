#!/bin/sh

# Generate kernel size statistics to be used with pgf-pie.
# Tested with tokei 12.1.2.
# Call from main directory.

cd kontroli/src/kernel
tokei -f *.rs -o json | \
  jq '.Total.children.Rust | map ({name, code: .stats.code})' | \
  sed \
    -e "s/reduce.rs/Reduction/" \
    -e "s/infer_check.rs/Typing/" \
    -e "s/subst.rs/Substitution/" \
    -e "s/sterm.rs/Terms/" \
    -e "s/convertible.rs/Convertibility/" \
    -e "s/mod.rs/API/" | \
  jq 'sort_by(.code) | reverse' | \
  jq -j '.[] | .code, "/", .name, ",\n"'

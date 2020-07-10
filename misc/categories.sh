#!/bin/sh

# Generate kernel size statistics to be used with pgf-pie.
# Call from main directory.

cd src/kernel
tokei -f * -o json | \
  jq '.inner.Rust.stats | map({code, name})' | \
  sed \
    -e "s/\(reduce\|matching\|state\).rs/Reduction/" \
    -e "s/typing.rs/Typing/" \
    -e "s/subst.rs/Substitution/" \
    -e "s/share.rs/Sharing/" \
    -e "s/rterm.rs/Terms/" \
    -e "s/convertible.rs/Convertibility/" \
    -e "s/mod.rs/API/" | \
  jq 'group_by(.name) | map({name: .[0].name, code: map(.code) | add})' | \
  jq 'sort_by(.code) | reverse' | \
  jq -j '.[] | .code, "/", .name, ",\n"'

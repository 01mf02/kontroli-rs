#!/bin/sh
# Create a flame graph from a run of kocheck with given arguments.
# Requires inferno (install with `cargo install inferno`), tested with version 0.10.13.
perf record --call-graph dwarf target/release/kocheck $@
perf script | inferno-collapse-perf | sed \
  -e 's/kontroli::[^;]*::\(infer\|check\);/infchk;/' \
  -e 's/kontroli::[^;]*::\(infer\|check\);//g' \
  -e 's/kontroli::[^;]*::\(infer\|check\)::..closure..;//g' \
  -e 's/kontroli::[^;]*::apply_subst;/appsub;/' \
  -e 's/kontroli::[^;]*::apply_subst;//g' \
  > stacks.folded
cat stacks.folded | inferno-flamegraph > flamegraph.svg

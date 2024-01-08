# Kontroli

![Build status](https://github.com/01mf02/kontroli-rs/workflows/Rust/badge.svg)
[![Crates.io](https://img.shields.io/crates/v/kontroli.svg)](https://crates.io/crates/kontroli)
[![Documentation](https://docs.rs/kontroli/badge.svg)](https://docs.rs/kontroli)
[![Rust 1.64+](https://img.shields.io/badge/rust-1.64+-orange.svg)](https://www.rust-lang.org)

Kontroli (Esperanto for *verify*) is
an alternative implementation of the logical framework [Dedukti],
concentrating on the verification of proofs.

Kontroli's use case is to
verify proofs generated by automated reasoning tools,
such as proof assistants and automated theorem provers.
It serves to verify the output of Dedukti by providing a "second opinion" and
to make it easier for users to understand and learn from the implementation.
It is also a testbed for parallelising type checking.

# Usage

Kontroli requires at least Rust 1.56.
Installation instructions are available at <https://rustup.rs/>.

To run Kontroli on output generated from Isabelle/Pure:

    cargo run --release examples/pure.dk

The command line option
`-j` enables concurrent checking and
`-c` enables concurrent parsing.

To install Kontroli:

    cargo install --path kocheck

Kontroli provides a command-line program, `kocheck`, and a library.
The latter means that you can use Kontroli as part of your own applications.
Given that the Kontroli library does not rely on Rust's standard library,
you could use it also in environments such as web pages,
offering type checking as a service.

# Goals

Kontroli tries to be the following:

* Small: write as little code as possible ...
* Correct: ... because the code you do not write, contains no bugs
* Efficient: be at least as fast as Dedukti in main use case
* Compatible: stick to Dedukti's syntax/semantics as much as possible
* Conservative: use established and well-tested techniques

# Differences

There are a few differences with respect to Dedukti:

* Kontroli has a module system allowing for nested modules.
* Kontroli does not support higher-order rewrite rules,
  as they would make the whole program considerably more complex,
  thus contradicting the idea of a small type checker.
* Kontroli assures the type-safety of rewrite rules only if
  all free pattern variables have type annotations.
* Kontroli does not use decision trees for rewriting.
* Kontroli does not have any commands like `#EVAL` or `#ASSERT`,
  which are particularly used in Dedukti tests.
  Instead, tests in the code base are preferred.

# Syntax

Kontroli implements a subset of Dedukti's syntax.
Examples of the syntax can be seen in
[examples/nat.dk](examples/nat.dk) or
[examples/pure.dk](examples/pure.dk).

Kontroli imposes that in a term with a subterm `(t)`, `t` must be a valid term itself.
This excludes one syntactic form allowed by Dedukti, namely `(a: A) -> b`,
because `a: A` on its own is not a proper term.
However, such terms can be written equivalently as `a: A -> b`.

# Development

A few useful commands for developing Kontroli:

* Generating documentation: `cargo doc --open`
* Running tests: `cargo test`
* Running benchmarks: `cargo bench -p kontroli -- --sample-size 10`


[Dedukti]: https://deducteam.github.io/

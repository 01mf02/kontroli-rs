# Kontroli

Kontroli (Esperanto for *verify*) is
an alternative implementation of the logical framework [Dedukti],
concentrating on the verification of proofs.

Kontroli's use case is to
verify machine-generated output of automated reasoning tools,
such as proof assistants and automated theorem provers.
It serves to verify the output of Dedukti by providing a "second opinion" and
to make it easier for users to understand and learn from the implementation.
It is also a testbed for parallelising type checking.

# Usage

To install Rust, follow the instructions on <https://rustup.rs/>.

To run Kontroli on output generated from Isabelle/Pure:

    cargo run --release examples/pure.ko

To install Kontroli:

    cargo install --path .

Kontroli provides a command-line program and a library.
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

* Kontroli's syntax is not left-recursive, in order to simplify parsing.
  As a result, Kontroli cannot directly read most of today's Dedukti files, but
  converting Kontroli to Dedukti files is only a matter of
  [a `sed` one-liner](#syntax).
  (Dedukti could be also easily extended to read/write Kontroli syntax.)
* Kontroli does not support higher-order rewrite rules,
  as they would make the whole program considerably more complex,
  thus contradicting the idea of a small type checker.
* Kontroli does not have a module system.
  However, when checking a sequence of files,
  any file can reference symbols from all files checked before,
  without prefixing the symbols with the file name.
* Kontroli does not try to assure the type-safety of rewrite rules.
  As a consequence, neither
  bracket patterns nor
  type annotations in rewrite rules are supported.
  However, using Kontroli's API, it should be possible
  to implement type checkers that assure type-safety.
* Kontroli does not use decision trees for rewriting.
* Kontroli does not have any commands like `#EVAL` or `#ASSERT`,
  which are particularly used in Dedukti tests.
  Instead, tests in the code base are preferred.

# Syntax

Kontroli implements a small subset of Dedukti's syntax,
modulo the addition of a prefix binder in front of
lambda abstractions and dependent products.
That is, a lambda abstraction
`x : A => y : B => x` in Dedukti becomes
`\ x : A => \ y : B => x` in Kontroli.
Furthermore, an independent product
`A -> B` in Dedukti becomes
`:A -> B` in Kontroli.
This eliminates [left-recursion](https://en.wikipedia.org/wiki/Left_recursion)
from the grammar, thus allowing for a greatly simplified parser in Kontroli.

Examples of the syntax can be seen in
[examples/nat.ko](examples/nat.ko) or
[examples/pure.ko](examples/pure.ko).

To obtain a Dedukti from a Kontroli file, use:

    sed -e 's/\\ //g' -e 's/! //g'  -e 's/:\([^ =]\)/\1/g' file.ko > file.dk

# Performance

On files generated from Isabelle (HOL.Inductive),
Dedukti takes 10.1 seconds to parse a 74MB DK file and
Kontroli takes 7.8 seconds to parse a 75MB KO file.

Dataset       | Size DK | Size KO | Parse DK | Parse KO
------------- | ------: | ------: | -------: | -------:
HOL.Inductive |    74MB |    75MB |    10.1s |     7.8s
HOL.List      |  2481MB |  2489MB |   386.4s |   192.2s

[Dedukti]: https://deducteam.github.io/

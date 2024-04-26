When we have a theory with many definitions that are used exactly one
(after having been declared), then we might want to eliminate these definitions
by inlining them, so that the signature during type checking remains small.

For this, we have to traverse our theory file twice:
first to find out which definitions are used exactly once, and
second to eliminate these definitions and to inline them.

The first step is done by `symcount`.
It actually counts for every constant how often it is used.
A count of 0 means that the constant was declared, but never used.

    cargo run --release --example symcount -- in.dk > in.symcount

To get those constants that are used exactly once, we use some UNIX magic:

    grep "^1[^0-9]" in.symcount | sed 's/^1.//' > in.inline

Finally, we use `inline`.
To save memory, this will forget any definition that has been inlined once,
so you currently cannot use this to inline definitions that are used more than once!

    cargo run --release --example inline -- in.dk in.inline > in.inlined.dk

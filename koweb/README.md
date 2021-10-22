Kontroli Web Interface
======================

First install `wasm-pack`:

    cargo install wasm-pack

Compile the WASM binaries (use the `--release` flag for vastly more performance):

    wasm-pack build --target web --release

To serve:

    python3 -m http.server --bind 127.0.0.1

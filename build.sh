#!/bin/sh

wasm32-wasi-cabal build

hs_wasm_path=$(find . -name "webeff.wasm")

"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs \
    --input "$hs_wasm_path" \
    --output pub/ghc_wasm_jsffi.js

cp "$hs_wasm_path" pub/bin.wasm

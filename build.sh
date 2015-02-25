#!/bin/sh
git submodule init
git submodule update
cabal update
ghc --version
cabal --version
cabal sandbox init
cabal install --only-dependencies --enable-tests
cabal configure --enable-tests && cabal build && cabal test

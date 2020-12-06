#!/bin/sh
set -xe

rm ./dist-newstyle/grow-vector-*-docs.tar.gz || true

# assumes cabal 2.4 or later
cabal v2-haddock --haddock-for-hackage --enable-doc

cabal upload -d --publish ./dist-newstyle/grow-vector-*.tar.gz

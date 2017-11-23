#!/bin/sh

set -e

echo haskell-servant
cd out/haskell-servant
ghc -fno-code *.hs
cd ../..

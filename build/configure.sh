#!/bin/bash

# This script is designed to be used inside Docker image.
# Please do not use it on Host machine.

if ! grep "split-objs: True" ~/.cabal/config; then echo "split-objs: True" >> ~/.cabal/config; fi

export PATH=~/.cabal/bin:$PATH

cabal --version
alex --version
happy --version

cd /opt/workspace
cabal sandbox init
cabal update
cabal install --only-dependencies --enable-tests --reorder-goals -j

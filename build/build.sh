#!/bin/bash

# This script is designed to be used inside Docker image.
# Please do not use it on Host machine.

cd /opt/workspace

export PATH=~/.cabal/bin:$PATH

cabal configure -f static-gmp

cabal build && cabal test

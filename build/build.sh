#!/bin/bash

# This script is designed to be used inside Docker image.
# Please do not use it on Host machine.

cd /opt/workspace

stack test --work-dir=.docker-stack-work --allow-different-user

stack build --no-test --flag carbon-aggregator:static-gmp --work-dir=.docker-stack-work --allow-different-user

mkdir -p dist
cp $(stack path --dist-dir --work-dir=.docker-stack-work --allow-different-user)/build/carbon-aggregator/carbon-aggregator dist/carbon-aggregator
rm -rf $(stack path --dist-dir --work-dir=.docker-stack-work --allow-different-user)/build/


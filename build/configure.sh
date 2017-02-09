#!/bin/bash

# This script is designed to be used inside Docker image.
# Please do not use it on Host machine.

cd /opt/workspace
stack setup --work-dir=.docker-stack-work --allow-different-user
stack clean --work-dir=.docker-stack-work --allow-different-user


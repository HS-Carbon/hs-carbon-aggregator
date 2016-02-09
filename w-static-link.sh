#!/usr/bin/env bash

if [[ "$@" == @* ]]; then
  RSP_FILE="$@"
  RSP_FILE=${RSP_FILE:1}
  RSP_ARGS=$(cat ${RSP_FILE} | sed -e 's/"-lgmp"/"-Wl,-Bstatic"\n"-lgmp"\n"-Wl,-Bdynamic"\n"-lpthread"/')
  echo -e "$RSP_ARGS" > ${RSP_FILE}
  gcc "@$RSP_FILE"
else
  CCARGS="$@"
  CLEANED_CCARGS=$(echo "$CCARGS" | sed -e 's/-lgmp//g')
  STATIC_GMP="-Wl,-Bstatic -lgmp -Wl,-Bdynamic -lpthread"
  gcc "$CLEANED_CCARGS $STATIC_GMP"
fi

#! /bin/sh -eux

# Original GHC command w/o GMP defined
CCARGS=$(echo "$@" | sed -e 's/-lgmp//g')

# GCC options to define GMP as static dependency
STATIC_GMP="-Wl,-Bstatic -lgmp -Wl,-Bdynamic -lpthread"

gcc $CCARGS $STATIC_GMP

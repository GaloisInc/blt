#!/bin/sh
# Convert fplll / NTL encoded matrices to Boost encoded.
#
# Usage: convert.sh dim1 dim2 < input > output

printf "[$1,$2]"
cat | sed 's/\[/(/g' \
    | sed 's/\]/)/g' \
    | sed 's/ \([-0-9]\)/,\1/g' \
    | tr -d '\n ' \
    | sed 's/)(/),(/g'

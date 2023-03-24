#!/bin/bash

python3 parser/src/main.py -f $1

cd interpreter

output_file="../${1}"

cabal run eng2fol-interpreter -- ${output_file}

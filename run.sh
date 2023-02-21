#!/bin/bash

echo "Enter path to text file to parse..."
read input_path

echo "Enter path to output folder..."
read output_path

python3 parser/src/main.py -in ${input_path} -out ${output_path}

cd interpreter

output_file="../${input_path%%.*}.json"

cabal run eng2fol -- ${output_file}

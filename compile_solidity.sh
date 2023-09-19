#!/bin/bash

#compiling full versions

full_contract_path="solidity/full"
#filename="auction"
#truffle compile $full_contract_path/$filename.sol
find "$full_contract_path" -type f -print | while read -r file; do
    filename_withExtension=$(basename "$file")
    filename="${filename_withExtension%.*}"
    echo "$filename"
    truffle compile $full_contract_path/$filename.sol
done
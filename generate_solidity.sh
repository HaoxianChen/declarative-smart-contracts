#!/bin/bash

#generating full versions
dl_path="benchmarks"
output_contract_path="solidity/full"
full_materialization_plan_path="view-materialization/full-set"
#filename="auction"
#sbt "run compile --materialize $full_materialization_plan_path/$filename.csv --out $output_contract_path $dl_path/$filename.dl"
find "$dl_path" -type f -print | while read -r file; do
    filename_withExtension=$(basename "$file")
    filename="${filename_withExtension%.*}"
    echo "$filename"
    sbt "run compile --materialize $full_materialization_plan_path/$filename.csv --out $output_contract_path $dl_path/$filename.dl"
done
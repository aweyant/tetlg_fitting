#!/bin/bash

cd data/livneh_unsplit/

file_list=(prec*.nc)

for file in "${file_list[@]}"
do
   ncea -O -d lat,27.0,52.0 -d lon,235.0,248.0 $file $file
done

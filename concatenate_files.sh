#!/bin/bash

cd data/livneh_unsplit/

file_list=(prec*.nc)

# SECTION NOT NEEDED
#'for file in "${file_list[@]}"
#'do
#'   ncap2 -O -s '@units="days since 1900-1-1 00:00:00";time=udunits(time,@units);time@units=@units' $file file
#'done

ncrcat -h --no_tmp_fl ${file_list[@]} complete.prec.nc

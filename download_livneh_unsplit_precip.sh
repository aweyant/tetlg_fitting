#!/bin/bash

for year in {1915..2018}
do
	#echo "https://downloads.psl.noaa.gov/Datasets/livneh/metvars/altprecip/prec.$year.nc" #>> file_list.txts
	wget "https://downloads.psl.noaa.gov/Datasets/livneh/metvars/altprecip/prec.$year.nc" --directory-prefix ./data/livneh_unsplit/
done

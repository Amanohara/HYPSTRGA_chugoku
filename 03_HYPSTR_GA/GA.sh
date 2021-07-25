#!/bin/bash
rm out_HYPST_2020m2.dat
count=`ls velocity* | wc -l`
for i in `seq 0 1 $((${count} - 1))`
do
    cp velocity${i} "./v_hl_theo.dat"
    timeout 5 ./HYPSTR_2020_m
done
# rm velocity*
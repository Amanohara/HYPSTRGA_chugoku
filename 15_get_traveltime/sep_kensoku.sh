#!/bin/bash
# 検測値を取得して任意のtraveltimeを計算する
#
username="TGIF"
password="naka2411"
#
date_file="hypo.dat"
hyponame=(`cat ${date_file}`)
hypocount=`cat ${date_file} | wc -l`
rm -R out_fl
mkdir out_fl

if [ "${hypocount}" -eq 0 ]; then
    end="0"
else
   end=$((${hypocount} - 1))
fi
for k in `seq 0 1 ${end} `
do
    date="${hyponame[k]:0:8}"
    year="${hyponame[k]:0:4}"
    month="${hyponame[k]:4:2}"
    month=`echo $((10#${month}))`
    day="${hyponame[k]:6:2}"
    day=`echo $((10#${day}))`
    # 検測値をDL
    python3 "./get_kensoku.py" ${username} ${password} ${year} ${month} ${day}
    # DLした検測値を地震毎に分割する
    input="./measure_${year}$(printf "%02d" ${month})$(printf "%02d" ${day})_1.txt"
    cp ${input} input_fl.dat
    ./JMA_data_sep
    # traveltimeを求める
    cp "./out_fl/J${hyponame[k]}" "./J${hyponame[k]}"
    python3 "./kensoku_divide.py" "J${hyponame[k]}"
    rm "./J${hyponame[k]}" ${input} input_fl.dat
done
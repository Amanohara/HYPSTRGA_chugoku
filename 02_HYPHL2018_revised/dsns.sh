#!/bin/bash
# -------------------------------------------------
# 観測点名を抜きだしデータセットを作る
# 読み取りデータ(カンマ区切りのもの)をinputフォルダに入れる
# -------------------------------------------------
# 組織ID	観測網ID	観測点コード	観測点名	観測点名よみ	観測点名英字	都道府県	設置緯度(世界測地系)	設置経度(世界測地系)	設置緯度(東京測地系)	設置経度(東京測地系)	地表面標高(m)	センサー標高(m)	掘削長(m)	KiK-net観測点コード	廃止観測点(1 = 廃止済み)
# -------------------------------------------------
# データの場所
data="./staiton/NIED_SeismicStation.dat"
input=(`ls ./input/*.dat`)
datacount=`ls ./input/*.dat | wc -l`
# for 文でループ
rm ./data/dsns*
for i in `seq 0 1 $((${datacount} - 1 ))`
do
    output="./data/dsns_${input[i]:8:12}.dat"
    rm ${output}
    obsname=(`sed -e '1,3d' ${input[i]} | awk -F"," '{print $1}' `)
    obscount=`sed -e '1,3d' ${input[i]} | awk -F"," '{print $1}' | wc -l`
    for j in `seq 0 1 $((${obscount} - 1 ))`
    do
        obsname2=${obsname[j]/'N.'/'N\.'}
        obslon=`grep ${obsname2} ${data} | awk -F"," '{print $8}'`
        obslat=`grep ${obsname2} ${data} | awk -F"," '{print $9}'`
        obsdepth=`grep ${obsname2} ${data} | awk -F"," '{print $13/1000}'`
        printf "%-9s %-9s %-9s %s\n" ${obsname[j]} ${obslon} ${obslat} ${obsdepth} >> ${output}
    done
done
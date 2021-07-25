#!/bin/bash
# ---------------------------------------------------
# 大渕さんの読み込みデータ群を震源決定用のデータにする。
# ファイル名を「(地震が発生した年月日時).dat」にしておくこと。
# ---------------------------------------------------
# 変換するデータは1つかそれ以上か( tanpin , multi)
result="multi"
if [ "${result}" == "tanpin" ]; then
    input="./result/result.dat"
    output="./converted/convert.dat"
    # エンコードがめちゃくちゃなのでutf-8にする。
    nkf -w --overwrite ${input[i]}
    # 先頭1行目はいらない & 変換作業
    sed -e '1d' ${input[i]} | awk -v OFS=, '{print "N."$1,$2,$3,"P",$4,$5,"S"}' > ${output}
elif [ "${result}" == "multi" ]; then
    input=(`ls ./result/*.dat`)
    datacount=`ls ./result/*.dat | wc -l`
    for i in `seq 0 1 $(($datacount -1))`
    do
        output="./converted/${input[i]:8:13}.dat"
        # エンコードがめちゃくちゃなのでutf-8にする。
        nkf -w --overwrite ${input[i]}
        # 先頭1行目はいらない & 変換作業
        sed  -e '1d' ${input[i]} | awk -v OFS=, '{print "N."$1,$2,$3,"P",$4,$5,"S"}' > ${output}
    done
else
    echo "なにかがおかしいよ"
fi
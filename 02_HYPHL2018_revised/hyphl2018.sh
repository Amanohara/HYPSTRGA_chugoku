#!/bin/bash
#
#   Programe for hypocenter determination
#
#
#   Setting of station coordinates from user's file
#
# 試行回数
nstop="100"
#
filename=(`ls ./input/*.dat`)
filecount=`ls ./input/*.dat | wc -l`
for k in `seq 0 1 $(($filecount - 1))`
do
    date=${filename[k]:8:8}
    time=${filename[k]:16:4}
    #
    stocfile='st_oc_'${date}${time}'.dat'
    hypofile='hypo_'${date}${time}'.dat'

    #
    stfile="data/dsns_${date}${time}.dat"
    #
    cat ${stfile} > dsns_2018.dat
    #
    #   Setting of seismic velocity data
    #
    velfile='data/dsnv.dat'
    #
    cat ${velfile} > dsnv2018

    #
    fldt='data/fldt'
    cat ${fldt} > fldt2018
    #
    #   Setting of pick data file names
    #
    #psdata='PSDATA.dat'

    psdata='input/'${date}${time}'.dat'

    cat ${psdata} > PSDATA2018.dat


    sed -e "s/NSTOP  =  100/NSTOP  =  ${nstop}/g" hyphl2018_orig.f > hyphl2018_revised.f
    rm hyphl2018
    gfortran "./hyphl2018_revised.f" -o "hyphl2018"
    ./hyphl2018

    #

    cp st_oc_2018.dat ${stocfile}
    cp hypo_2018.dat ${hypofile}

    #rm in_hypo.dat
    #rm in_psdata.dat
    rm "st_oc_${date}${time}.dat"
    rm "hypo_${date}${time}.dat"
    rm PSDATA.dat
    # rm PSDATA2018.dat
    rm dsnv2018
    rm dsns_2018.dat
    rm fldt2018
    rm hypo_2018.dat
    rm st_oc_2018.dat

    mv hypout2018all.txt "${date}${time}_all.txt"
done
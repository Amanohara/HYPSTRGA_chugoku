#!/bin/bash

cat EIN2018_Kuma.dat > efile_in.dat

./artini_Kuma

# 地震初期値作終了

rm efile_in.dat

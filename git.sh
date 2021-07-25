#!/bin/bash
hoge=`date "+%Y%m%d-%H%M%S"`
git add .
git commit -m "${hoge}"
git push origin master

#!/bin/bash

path=$1

if [ "$path" == "" ] 
then 
    files=$(ls *.eps)
else
    files=$(ls $path/*.eps)
fi

for file in $files; do 
    dir=$(dirname "${file}")
    base=`exec basename ${file} .eps`;
    `convert -density 300 ${file} -resize 25% ${dir}/${base}.png`;
    echo " +++ ${dir}/${base}.png DONE"
    `rm -f ${file}`;
    echo " --- ${file} REMOVED"
done

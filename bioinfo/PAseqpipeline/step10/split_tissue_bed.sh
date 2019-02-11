#!/bin/sh

chr=($(seq 1 22) "X" "Y")

for tissue in `ls /mnt/data2/songyf/project/apa13tissue/3step`
do
	mkdir $tissue/minus
	mkdir $tissue/plus

	for x in ${chr[@]}
	do
		gawk -v chri=chr$x '$1==chri' $tissue/${tissue}_pl_R1.bed > $tissue/plus/chr$x
		gawk -v chri=chr$x '$1==chri' $tissue/${tissue}_min_R1.bed > $tissue/minus/chr$x
	done
done

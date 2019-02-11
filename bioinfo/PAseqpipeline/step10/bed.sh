#!/bin/sh
for tissue in `ls /mnt/data2/songyf/project/apa13tissue/3step`
do 
	perl PA_minusstrand.pl $tissue/${tissue}_min_R1.bed $tissue/${tissue}_min_R1_PA.bed
	perl PA_plusstrand.pl $tissue/${tissue}_pl_R1.bed $tissue/${tissue}_pl_R1_PA.bed
done

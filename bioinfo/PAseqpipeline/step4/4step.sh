#!/bin/sh
#把这个脚本移到4step文件夹内执行
mkdir bed
mkdir minbed
mkdir plbed
for sample in `ls ../3step`
do
  bedtools bamtobed -bed12 -split -i ../3step/$sample/accepted_hits.bam > bed/${sample}.bed
  gawk 'BEGIN{FS="\t"};($4~/\/1/ && $6=="+") || ($4~/\/2/ && $6=="-")' bed/${sample}.bed > minbed/${sample}_min.bed
  gawk 'BEGIN{FS="\t"};($4~/\/2/ && $6=="+") || ($4~/\/1/ && $6=="-")' bed/${sample}.bed > plbed/${sample}_pl.bed
done

cat minbed/* > min.bed
cat plbed/* > pl.bed
# get R1
grep "\/1" min.bed > minr1.bed
grep "\/1" pl.bed > plr1.bed

rm min.bed pl.bed

perl PA_minusstrand.pl minr1.bed minr1pa.bed
perl PA_plusstrand.pl plr1.bed plr1pa.bed

sort -k1,1d -k2,2n -k3,3n minr1pa.bed > sortminr1pa.bed
sort -k1,1d -k2,2n -k3,3n plr1pa.bed > sortplr1pa.bed

awk 'length($1)<6' sortplr1pa.bed > filter.sortplr1pa.bed
awk 'length($1)<6' sortminr1pa.bed > filter.sortminr1pa.bed
rm sortminr1pa.bed
rm sortplr1pa.bed


bedtools bedtobam -i filter.sortminr1pa.bed -g /mnt/data2/genome/hg19_refseq/hg19_chromosome.size.txt > sortminr1pa.bam
bedtools bedtobam -i filter.sortplr1pa.bed -g /mnt/data2/genome/hg19_refseq/hg19_chromosome.size.txt > sortplr1pa.bam

samtools mpileup -EB -I -f /mnt/data2/genome/hg19_refseq/hg19.fa sortplr1pa.bam > plus.mpileup
samtools mpileup -EB -I -f /mnt/data2/genome/hg19_refseq/hg19.fa sortminr1pa.bam > minus.mpileup

gawk 'BEGIN{OFS="\t"};{if ($4 != 0) {print $1,$2,$3,$4,"+"}}' plus.mpileup > plus.count.on.genome
gawk 'BEGIN{OFS="\t"};{if ($4 != 0) {print $1,$2,$3,$4,"+"}}' minus.mpileup > minus.count.on.genome

mkdir fseq
mkdir fseq/minus
mkdir fseq/plus
/home/yaojun/software/fseq/bin/fseq -f 1 -l 30 -of npf -o fseq/minus filter.sortminr1pa.bed
/home/yaojun/software/fseq/bin/fseq -f 1 -l 30 -of npf -o fseq/plus filter.sortplr1pa.bed


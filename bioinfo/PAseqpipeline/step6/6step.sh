#!/bin/bash
cat ../5step/*minus_peak_mode > minus.peak
cat ../5step/*plus_peak_mode > plus.peak
gawk '$13>0' plus.peak > filter.plus.peak 
gawk '$13>0' minus.peak > filter.minus.peak
perl get_seq_minus.pl filter.minus.peak down20.minus.peak
perl get_seq_plus.pl filter.plus.peak down20.plus.peak
bedtools getfasta -fi /mnt/data2/genome/hg19_refseq/hg19.fa -bed down20.minus.peak -s -name -fo down20.minus.peak.fa
bedtools getfasta -fi /mnt/data2/genome/hg19_refseq/hg19.fa -bed down20.plus.peak -s -name -fo down20.plus.peak.fa
perl add_seq.pl down20.minus.peak.fa filter.minus.peak down20seq.minus
perl add_seq.pl down20.plus.peak.fa filter.plus.peak down20seq.plus
perl filter_A.pl down20seq.plus down20seq.plus.arich down20seq.plus.noarich
perl filter_A.pl down20seq.minus down20seq.plus.arich down20seq.minus.noarich

gawk '$15>=20' down20seq.minus.noarich > ../7step/minus
gawk '$15>=20' down20seq.plus.noarich > ../7step/plus
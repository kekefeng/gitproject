#!/usr/bin/perl
use Math::Round;
my $file_name=shift;
open RSD,"<",$file_name;

my $first_line=<RSD>;
while (<RSD>){
	my ($sample,$r,$sd,$fq1,$fq2)=split /\t/;
	my $r=round $r;
	my $sd=round $sd;
	system "tophat2 --bowtie1 -g 1 --transcriptome-index=/mnt/data2/genome/hg19_refseq/transcriptome_refseq/trans_bowtie --library-type=fr-firststrand -r $r --mate-std-dev $sd -p 12 -o /mnt/data2/songyf/project/apa13tissue/3step/$sample /mnt/data2/songyf/index/hg19/bowtie/index $fq1 $fq2";
	system "bedtools bamtobed -bed12 -split -i accepted_hits.bam > accepted_hits.bed";
	system(q{gawk 'BEGIN{FS="\t"};($4~/\/1/ && $6=="+") || ($4~/\/2/ && $6=="-")' accepted_hits.bed > minus.bed});
	system(q{gawk 'BEGIN{FS="\t"};($4~/\/2/ && $6=="+") || ($4~/\/1/ && $6=="-")' accepted_hits.bed > plus.bed});
}
system 'cat */minus.bed | grep "\/1" > minus_R1.bed';
system 'cat '
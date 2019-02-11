#!/usr/bin/perl
use 5.012;
my @prechrs=(1..22,"X","Y");
my @chrs= map {"chr".$_} @prechrs;
my $MinusGtf = "hg19_refseq_NM_NR_minus_remodel.txt";
my $PlusGtf = "hg19_refseq_NM_NR_plus_remodel.txt";
my $PlusNoA= "../7step/plus";
my $MinusNoA= "../7step/minus";

mkdir "splitgtf";
mkdir "NoAsplit";
mkdir "peak2gene";
mkdir "peak2gene/minus";
mkdir "peak2gene/plus";

foreach my $chr(@chrs){
    system "gawk '\$5==\"$chr\"' $MinusGtf > splitgtf/minus.$chr";
    system "gawk '\$5==\"$chr\"' $PlusGtf > splitgtf/plus.$chr";
    system "gawk '\$1==\"$chr\"' $PlusNoA > NoAsplit/plus.$chr";
    system "gawk '\$1==\"$chr\"' $MinusNoA > NoAsplit/minus.$chr";
    system "perl Peak_map_to_gene_update.pl splitgtf/minus.$chr NoAsplit/minus.$chr peak2gene/minus/$chr";
    system "perl Peak_map_to_gene_update.pl splitgtf/plus.$chr NoAsplit/plus.$chr peak2gene/plus/$chr";
}

system "cat peak2gene/minus/* > minus";
system "cat peak2gene/plus/* > plus";


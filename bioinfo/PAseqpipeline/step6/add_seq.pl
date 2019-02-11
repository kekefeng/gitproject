#!/usr/bin/perl -w
use strict;
open(SEQ20,"< $ARGV[0]")
    or die "Can not open the file\n";
open(MODE,"< $ARGV[1]")
    or die "Can not open the file\n";
open(SEQ,"> $ARGV[2]")
    or die "Can not open the file\n";
my @mode=();
while (<MODE>){
chomp($_);
next if (/^V1/);
push @mode,$_;
}
my @seq=();
while (<SEQ20>){
chomp($_);
push @seq,$_;
}
for (my $i=0;$i<=$#mode;$i++)
{
	print SEQ "$mode[$i]\t$seq[$i*2+1]\n";
}
close (SEQ20);
close (MODE);
close (SEQ);
   
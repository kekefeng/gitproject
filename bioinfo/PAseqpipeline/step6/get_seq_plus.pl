#!/usr/bin/perl -w
use strict;
open(MODE,"< $ARGV[0]")
    or die "Can not open the file\n";
open(MODE_A,"> $ARGV[1]")
    or die "Can not open the file\n";
my @line=();
while (<MODE>){
chomp($_);
next if (/^V1/);
@line=split /\s+/, $_;
my $end_site=$line[12]+20;
my $strand="+";
print MODE_A "$line[0]\t$line[12]\t$end_site\t$line[3]\t0\t$strand\n";
}
close(MODE);
close(MODE_A);

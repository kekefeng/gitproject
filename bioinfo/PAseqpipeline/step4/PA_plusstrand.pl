#!/usr/bin/perl -w
use strict;
open(ALL_SITE,"< $ARGV[0]") or die $!;
open(PA,"> $ARGV[1]") or die $!;
#get max value
#generate a hash table
while (<ALL_SITE>)
{
	chomp($_);
	my @lines=split (/\t/, $_);
	$lines[1]=$lines[2]-1;
	my $line=join "\t",@lines;
	print PA "$line\n";
}
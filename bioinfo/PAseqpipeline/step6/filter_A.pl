#!/usr/bin/perl -w
use strict;
open(SEQ,"< $ARGV[0]")
    or die "Can not open the file\n";
open(A_RICH_SEQ,"> $ARGV[1]")
    or die "Can not open the file\n";
open(NO_A_RICH_SEQ,"> $ARGV[2]")
    or die "Can not open the file\n";
my @line=();
my @temp_seq=();
while (<SEQ>){
chomp($_);
next if (/start/);
@line=split /\s+/, $_;
@temp_seq=split //,$line[23];
my $count_A_15=0;
my $count_A_6=0;
foreach my $item1 (@temp_seq)
{
	if ($item1 eq "A" or $item1 eq "a")
	{
		$count_A_15=$count_A_15+1;
	}
}
#
my $i=0;
for ($i=0;$i<=5;$i++)
{
	if ($temp_seq[$i] eq "A" or $temp_seq[$i] eq "a")
	{
		$count_A_6=$count_A_6+1;
	}
}
#
if (not ($count_A_15>=15 or $count_A_6==6))
{
print NO_A_RICH_SEQ "$_\n";
}
else
{
print A_RICH_SEQ "$_\n";
}
}
close(SEQ);
close(A_RICH_SEQ);
close(NO_A_RICH_SEQ);


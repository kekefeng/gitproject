#!/usr/bin/perl -w
use strict;
open(ALL_SITE,"< $ARGV[0]")
    or die "Can not open the file\n";
open(PA,"> $ARGV[1]")
    or die "Can not open the file\n";
#get max value
sub max
{
my @line=@_;
my $max_so_far =$line[0];
my $max_so_far_id=0;
foreach (0..$#line)
{
if ($line[$_] > $max_so_far)
{
$max_so_far=$line[$_];
$max_so_far_id=$_;
}
}
$max_so_far_id;
}
#generate a hash table
my @line=();
my %line=();
my $line=();
while (<ALL_SITE>)
{
chomp($_);
@line=split (/\s+/, $_);
push @{$line{$line[3]}},$_;
}
#get max end
foreach my $key (sort keys %line)
{
my @end_site=();
if ($#{$line{$key}}>=1)
{
foreach my $item (@{$line{$key}})
{
chomp($item);
@line=split (/\s+/, $item);
push @end_site,$line[2];
}
my $max_end_site_id=&max(@end_site);
${$line{$key}}[0]=${$line{$key}}[$max_end_site_id];
}
}
#get PA site for plus strand
foreach my $key (sort keys %line)
{
@line=split (/\s+/, ${$line{$key}}[0]);
$line[1]=$line[2]-1;
$line=join "\t",@line;
print PA "$line\n";
}
close(ALL_SITE);
close(PA);
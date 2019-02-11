#!/usr/bin/perl -w
## 混合了13个样品后
## 有些read匹配不止一次，如果有多行匹配，从中选择起始位置最小的那一行，
## 并且把对应的结束位置改成start+1的值
use strict;
open(ALL_SITE,"< $ARGV[0]")
    or die "Can not open the file\n";
open(PA,"> $ARGV[1]")
    or die "Can not open the file\n";
#get min value
sub min{
	my @line=@_;
	my $min_so_far =$line[0];
	my $min_so_far_id=0;
	foreach (0..$#line){
		if ($line[$_] < $min_so_far)
		{
			$min_so_far=$line[$_];
			$min_so_far_id=$_;
		}
	}
	$min_so_far_id;
}
#generate a hash table
my @line=();
my %line=();
my $line=();
while (<ALL_SITE>)
{
	chomp($_);
	@line=split (/\s+/, $_);
	push @{$line{$line[3]}},$_; #用数组的第四个值作为哈希的键，值为数组指针
}
#get min start
foreach my $key (sort keys %line)
{
	my @start_site=();
	if ($#{$line{$key}}>=1){
		foreach my $item (@{$line{$key}})
		{
			chomp($item);
			@line=split (/\s+/, $item);
			push @start_site,$line[1];
		}
		my $min_end_site_id=&min(@start_site);
		${$line{$key}}[0]=${$line{$key}}[$min_end_site_id];
	}
}
#get PA site for minus strand
foreach my $key (sort keys %line)
{
	@line=split (/\s+/, ${$line{$key}}[0]);
	$line[2]=$line[1]+1;
	$line=join "\t",@line;
	print PA "$line\n";
}
close(ALL_SITE);
close(PA);
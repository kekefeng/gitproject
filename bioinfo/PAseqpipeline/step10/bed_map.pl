#!/usr/bin/perl -w
use strict;
open(BED,"< $ARGV[0]")
    or die "Can not open the file\n";
open(GENE_PEAK,"< $ARGV[1]")
    or die "Can not open the file\n";
open(GENE_PEAK_BED,"> $ARGV[2]")
    or die "Can not open the file\n";
open(UPDATE_BED,"> $ARGV[3]")
    or die "Can not open the file\n";

my @beds=<BED>;
#
my @peak_line;
my @bed_line;

#my $i=0;
while (<GENE_PEAK>) {
	chomp($_);
	@peak_line=split /\s+/, $_;
	my $count=0;
	
	foreach my $bedline (@beds){
		my @split_bed=split /\s+/, $bedline;	
		if ($split_bed[2]>=$peak_line[16] && $split_bed[2]<=$peak_line[17]){
			$count=$count+1;
			print UPDATE_BED join "\t",@split_bed[0..9],"\n";
		}
	}
	print GENE_PEAK_BED "$_\t$count\n";
#	$i++;
#	print $i,"\n";
}

close(BED);
close(GENE_PEAK);
close(GENE_PEAK_BED);
close(UPDATE_BED);

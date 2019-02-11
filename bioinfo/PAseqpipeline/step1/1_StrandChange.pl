#! /usr/bin/perl

use 5.012;
open FQ1,"<","$ARGV[0]" or die $!;
open FQ2,"<","$ARGV[1]" or die $!;

open OUT1,">","$ARGV[2]" or die $!;
open OUT2,">","$ARGV[3]" or die $!;

my $fore1;
my $fore2;

while (my $name1=<FQ1>){
	my $seq1  =<FQ1>;
	my $three1=<FQ1>;
	my $quali1=<FQ1>;
	
	my $name2   =<FQ2>;
	my $seq2  =<FQ2>;
	my $three2=<FQ2>;
	my $quali2=<FQ2>;

	if ($seq1=~/^TTT/ && $seq2!~/^TTT/){
		$seq1  =substr($seq1  ,3);
		$quali1=substr($quali1,3);
		print OUT1 join "",$name1,$seq1,$three1,$quali1;
		print OUT2 join "",$name2,$seq2,$three2,$quali2;
	}elsif($seq2=~/^TTT/ && $seq1!~/^TTT/){
		$seq2  =substr($seq2  ,3);
		$quali2=substr($quali2,3);
		print OUT1 join "",$name2,$seq2,$three2,$quali2;
		print OUT2 join "",$name1,$seq1,$three1,$quali1;
	}
}

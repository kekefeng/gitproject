use 5.012;
my $sample=shift;
my $openfile=$sample.".ljb26.hg19_multianno.txt";
my $outfile=$sample.".ljb26.out";
open IN,"<",$openfile or die $!;
my $title=<IN>;
my %hash;
my @t=qw(sift_pred Polyphen2_HDIV_pred Polyphen2_HVAR_pred LRT_pred MutationTaster_pred MutationAssessor_pred FATHMM_pred RadialSVM_pred);
while (<IN>){
	chomp;
	my ($sift_pred,$Polyphen2_HDIV_pred,$Polyphen2_HVAR_pred,$LRT_pred,$MutationTaster_pred,$MutationAssessor_pred,$FATHMM_pred,$RadialSVM_pred)=(split /\t/)[6,8,10,12,14,16,18,20];
	$hash{"sift_pred"}->{$sift_pred}++;
	$hash{$t[1]}->{$Polyphen2_HDIV_pred}++;
	$hash{$t[2]}->{$Polyphen2_HVAR_pred}++;
	$hash{$t[3]}->{$LRT_pred}++;
	$hash{$t[4]}->{$MutationTaster_pred}++;
	$hash{$t[5]}->{$MutationAssessor_pred}++;
	$hash{$t[6]}->{$FATHMM_pred}++;
	$hash{$t[7]}->{$RadialSVM_pred}++;
}

open OUT,">",$outfile or die $!;
say OUT "type\tNumber\tDeleterious";
say OUT "$t[0]\t$hash{$t[0]}->{T}\t$hash{$t[0]}->{D}";

my $c=$hash{$t[1]}->{P}+$hash{$t[1]}->{D};
say OUT "$t[1]\t$hash{$t[1]}->{B}\t$c";

$c=$hash{$t[2]}->{P}+$hash{$t[2]}->{D};
say OUT "$t[2]\t$hash{$t[2]}->{B}\t$c";

say OUT "$t[3]\t$hash{$t[3]}->{N}\t$hash{$t[3]}->{D}";

say OUT "$t[4]\t$hash{$t[4]}->{P}\t$hash{$t[4]}->{D}";
$c=$hash{$t[5]}->{H}+$hash{$t[5]}->{L}+$hash{$t[5]}->{M};
say OUT "$t[5]\t$hash{$t[5]}->{N}\t$c";
say OUT "$t[6]\t$hash{$t[6]}->{T}\t$hash{$t[6]}->{D}";
say OUT "$t[7]\t$hash{$t[7]}->{T}\t$hash{$t[7]}->{D}";
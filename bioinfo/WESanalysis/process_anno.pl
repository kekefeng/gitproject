use 5.012;
my $sample=shift;
my $openfile=$sample.".indel.hg19_multianno.txt";
my $outfile=$sample."_indel_anno_process.out";

open IN,"<",$openfile or die $!;
readline IN;
my $snpcount;
my ($allcount,$allmaf);
my ($espcount,$espmaf);
while (<IN>){
	chomp;
	my ($snp,$all,$esp)=(split /\t/)[10,11,17];

	if($snp=~/^rs/){
		$snpcount++;
	}
	if(not $all eq '.'){
		$allcount++;
		$allmaf+=$all;
	}
	if(not $esp eq '.'){
		$espcount++;
		$espmaf+=$esp;
	}
}
$allmaf=int($allmaf);
$espmaf=int($espmaf);
open OUT,">",$outfile or die $!;
say OUT "Database\tNumber\tMAF";
say OUT "dbSNP\t$snpcount\t-";
say OUT "1000G_ALL\t$allcount\t$allmaf";
say OUT "ESP6500siv2_ALL\t$espcount\t$espmaf";
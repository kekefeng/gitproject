use 5.012;
my @samples=qw(BM_GFP Ctrl M9_D11 M9_D19 VCR_D8 VCR_D13);
my %tpm;
foreach my $sample(@samples){
	my $file=$sample.".abun";
	say $file;
	open IN,"<",$file or die $!;
	readline IN;
	while(<IN>){
		chomp;
		my ($symbol,$tpm)=(split /\t/)[1,8];
		if(exists $tpm{$sample}->{$symbol}){
			delete $tpm{$sample}->{$symbol};
		}else{
			$tpm{$sample}->{$symbol}=$tpm;
		}
	}
	close IN;
}

open IN,"<","intersectgene" or die $!;
my %unigene;
while(<IN>){
	chomp;
	$unigene{$_}=1;
}

open OUT,">","all.abun" or die $!;
say OUT join "\t","genesymbol",@samples;
print $tpm{"BM_GFP"}->{Gm1357};
foreach my $gene(sort keys %unigene){
	print OUT $gene;
	my $expre;
	foreach my $sample(@samples){
		if(exists $tpm{$sample}->{$gene}){
			$expre.="\t$tpm{$sample}->{$gene}";
		}else{
			$expre.="\t0";
		}
	}
	if ($expre eq "\t0\t0\t0\t0\t0\t0"){
		say $gene;
		next;
	}else{
		print OUT "$expre\n";
	}
}
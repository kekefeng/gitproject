use 5.012;
my @samples=qw(GFP_Ctrl GFP_D11 GFP_D19 VCR_D8 VCR_D13);
my %unigene;
open IN,"<","intersectgene" or die $!;
my %unigene;
while(<IN>){
	chomp;
	$unigene{$_}=1;
}

foreach my $file(@samples){
	open IN,"<",$file or die $!;
	open OUT,">",$file.".new" or die $!;
	while (<IN>){
		chomp;
		if (exists $unigene{$_}){
			say OUT;
		}
	}
	close OUT;
	close IN;
}
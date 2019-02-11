use 5.012;
my @samples=qw(HYF XCF XCL XLY XN XR XYZ);

open ALLONE,">","indelforR1" or die $!; 
say ALLONE "type\tnumber\tsample";
open ALLTWO,">","indelforR2" or die $!;
say ALLTWO "type\tPercentage\tSample";

foreach my $sample(@samples){
	my $openfile=$sample.".indel.hg19_multianno.txt";
	my $GOfile=$sample."_for_go_indel.txt";
	my $outfile=$sample."_indel_process.out";
	my $lengthfile=$sample.".length";
	open IN,"<",$openfile or die $!;
	readline IN;
	my %hash;
	my %indelmode;

	open GO,">",$GOfile or die $!;
	my $totalnum;
	while (<IN>){
		chomp;
		my ($ref,$alt,$pos_type,$gene,$mean_type)=(split /\t/)[3,4,5,6,8];
		$hash{pos}->{$pos_type}++;   #1.1
		$totalnum++;
		my $len=length($ref)+length($alt)-1;
		$len=7 if ($len>6);
		my $exonflag= ($pos_type=~/^exonic/);
		if($exonflag){
			$hash{mean}->{$mean_type}++; #1.2
		# 1.3.2
			$indelmode{coding}->{$len}++;
			
			if($mean_type eq "frameshift insertion"){
				my @tmps=split /,/,$gene;
				foreach (@tmps){
					say GO;
				}
			}
		}
		
	 #1.3.1
		$indelmode{genome}->{$len}++;
	}


	open OUT,">",$outfile or die $!;
	#hash:pos mean
	#1	基因组不同区域上INDEL的分布
	say OUT "indel_pos\tnumber\tpercentage";
	foreach my $pos_type(sort keys %{$hash{pos}}){
		my $ratio=$hash{pos}->{$pos_type}/$totalnum;
		$ratio=sprintf("%.2f", $ratio);
		say OUT "$pos_type\t$hash{pos}->{$pos_type}\t$ratio";
		say ALLONE "$pos_type\t$hash{pos}->{$pos_type}\t$sample";
	}
	say OUT "";

	#2	外显子区INDEL功能注释及统计
	say OUT "type\tnumber\tpercentage";
	my $meannum;
	foreach my $mean_type(sort keys %{$hash{mean}}){
		$meannum+=$hash{mean}->{$mean_type} if ($mean_type!~/\./);
	}

	foreach my $mean_type(keys %{$hash{mean}}){
		my $ratio=$hash{mean}->{$mean_type}/$meannum;
		$ratio=sprintf("%.2f", $ratio);	
		if ($mean_type!~/\./){
			say OUT "$mean_type\t$hash{mean}->{$mean_type}\t$ratio";
			say ALLTWO "$mean_type\t$hash{mean}->{$mean_type}\t$sample";
		}
	}
	say OUT "";

	#3 INDEL突变模式分布统计
	open LEN,">",$lengthfile or die $!;
	say LEN "INDEL length\tGenome\tExonic";
	foreach my $len(1..6){
		say LEN "$len\t$indelmode{genome}->{$len}\t$indelmode{coding}->{$len}";
	}
	say LEN ">6\t$indelmode{genome}->{7}\t$indelmode{coding}->{7}";


	#
}
#生成一个大文件。
#同时生成小文件，为画图用
use 5.012;
my @samples=qw(HYF XCF XCL XLY XN XR XYZ);

open REGDIS,">","region.distribution" or die $!; 
open FUNCANNO,">","snpforR1" or die $!; 
say FUNCANNO "type\tnumber\tsample";
open ALLTWO,">","snpforR2" or die $!;
foreach my $sample(@samples){
	my $openfile=$sample.".snp.hg19_multianno.txt";
	my $GOfile="go/".$sample."_for_go_snp.txt";
	my $outfile="pro/".$sample."_snp_process.out";
	open IN,"<",$openfile or die $!;
	readline IN;
	my %hash;
	my %trans;
	my %type=('A-G',1,'C-T',1,'T-C',1,'G-A',1);

	open GO,">",$GOfile or die $!;
	my $totalnum;
	while (<IN>){
		chomp;
		my ($ref,$alt,$pos_type,$gene,$mean_type)=(split /\t/)[3,4,5,6,8];
		$hash{pos}->{$pos_type}++;   #1.1
		$totalnum++;
		my $snp=$ref."-".$alt; 
		my $exonflag= ($pos_type=~/^exonic/);
		if($exonflag){
			$hash{mean}->{$mean_type}++; #1.2
		# 1.3.2
			if(exists $type{$snp}){
				$trans{coding}->{sitions}++;
			}elsif($snp!~/0/){
				$trans{coding}->{versions}++;
			}
			if($mean_type eq "nonsynonymous SNV"){
				my @tmps=split /,/,$gene;
				foreach (@tmps){
					say GO;
				}
			}
		}
		
	 #1.3.1
		if(exists $type{$snp}){
			$trans{genome}->{sitions}++;
		}else{
			$trans{genome}->{versions}++;
		}

		$hash{snp}->{$snp}++;    #1.4
		
	}


	open OUT,">",$outfile or die $!;
	say OUT "snp_type\tnumber\tpercentage";
	say REGDIS "snp_type\tnumber\tpercentage";
	#hash:pos mean snp
	#输出SNP在基因组上不同类型的分布情况 图1
	foreach my $pos_type(sort keys %{$hash{pos}}){
		my $ratio=$hash{pos}->{$pos_type}/$totalnum;
		$ratio=sprintf("%.2f", $ratio);
		say OUT "$pos_type\t$hash{pos}->{$pos_type}\t$ratio";
		say REGDIS "$pos_type\t$hash{pos}->{$pos_type}\t$ratio";
	}
	say OUT "";
	close REGDIS；
	
	#外显子区域的SNP突变类型统计
	say OUT "type\tnumber\tpercentage";
	my $meannum;
	foreach my $mean_type(sort keys %{$hash{mean}}){
		$meannum+=$hash{mean}->{$mean_type} if ($mean_type!~/\./);
	}

	foreach my $mean_type(sort keys %{$hash{mean}}){
		my $ratio=$hash{mean}->{$mean_type}/$meannum;
		$ratio=sprintf("%.2f", $ratio);	
		if ($mean_type!~/\./){
			say FUNCANNO "$mean_type\t$hash{mean}->{$mean_type}\t$sample";
			say OUT "$mean_type\t$hash{mean}->{$mean_type}\t$ratio";
		}
	}
	say OUT "";

	#TS/TV 不画图

	say OUT "TS genome\t$trans{genome}->{sitions}";
	say OUT "TV genome\t$trans{genome}->{versions}";
	my $TS_TV_genome=$trans{genome}->{sitions}/$trans{genome}->{versions};
	print OUT "TS/TV genome\t";
	say OUT sprintf("%.2f", $TS_TV_genome);

	say OUT "TS exonic\t$trans{coding}->{sitions}";
	say OUT "TV exonic\t$trans{coding}->{versions}";
	my $TS_TV_exonic=$trans{coding}->{sitions}/$trans{coding}->{versions};
	print OUT "TS/TV exonic\t";
	say OUT sprintf("%.2f", $TS_TV_exonic);
	say OUT "";

	#
	say OUT "";
	say OUT "snp\tnumber";
	my $cg=$hash{snp}->{"C-G"}+$hash{snp}->{"G-C"};
	say OUT "C-G\t$cg";
	say ALLTWO "C-G\t$cg\t$sample";
	my $ct=$hash{snp}->{"C-T"}+$hash{snp}->{"T-C"};
	say OUT "C-T\t$ct";
	say ALLTWO "C-T\t$ct\t$sample";
	my $ca=$hash{snp}->{"C-A"}+$hash{snp}->{"A-C"};
	say OUT "C-A\t$ca";
	say ALLTWO "C-A\t$ca\t$sample";
	my $tg=$hash{snp}->{"T-G"}+$hash{snp}->{"G-T"};
	say OUT "T-G\t$tg";
	say ALLTWO "T-G\t$tg\t$sample";
	my $tc=$hash{snp}->{"T-C"}+$hash{snp}->{"C-T"};
	say OUT "T-C\t$tc";
	say ALLTWO "T-C\t$tc\t$sample";
	my $ta=$hash{snp}->{"T-A"}+$hash{snp}->{"A-T"};
	say OUT "T-A\t$ta";
	say ALLTWO "T-C\t$ta\t$sample";

	say OUT "";
}

#!/usr/bin/perl
#usage : cd to the fq.gz folder 
#perl 2step.pl desfolder samplename xx1.fq xx2.fq ...   or xx1.gz xx2.gz
#the samplename 文件中一行一个xx和作为参数输入的 xx1/2.fq 是次序一致的
#gunzip fastq ,bowtie and awk to get r&sd result
#修改过了，第三步直接读取samplelist文件跑tophat
use 5.012;

my $desfolder=shift;
my $samplefile=shift;
my @sample=`cat $samplefile`;
chomp @sample;
my $log=$desfolder."/log";
open OUT,">",$log;
open SMLI,">","samplelist"; 

while (my $sample=shift @sample){
	my $gz1=shift;	my $gz2=shift;
	my ($fq1,$fq2);
	if ($gz1 =~ /\.gz$/){
		($fq1,$fq2)=&gunzip($gz1,$gz2);  
	}else{
		$fq1=$gz1;
		$fq2=$gz2;
	}  #如果输入的是gz格式的文件，解压
	
	say OUT '------------------------';
	say OUT join "\t",$sample,$fq1,$fq2;
	my $bowtieout=`bowtie -p 12 --chunkmbs 2000 /mnt/data2/songyf/index/hg19/bowtie/index -1 $fq1 -2 $fq2 -S $desfolder/${fq2}.sam`;
	say OUT $bowtieout;
	print time(),"\n";
	my $awkresult=`awk '{if (\$9 >0) {sum+=\$9;sumsq+=\$9*\$9;N+=1}} END {print "mean = " sum/N " SD=" sqrt(sumsq/N - (sum/N)**2)}' $desfolder/${fq2}.sam`;
	say OUT $awkresult;
	$awkresult=~/mean = (.*?) SD=(.*)\n/;
	my $r=int($1+0.5);
	my $sd=int($2+0.5);   #round
	say SMLI join "\t",$sample,$r,$sd,$fq1,$fq2;
	system "samtools view -Sb $desfolder/${fq2}.sam > $desfolder/${fq2}.bam";
	system "rm $desfolder/${fq2}.sam";
}
close OUT;
close SMLI;

sub gunzip{
	my $gz1=shift;
	my $gz2=shift;
	
	my $pid=fork();
	if ($pid){
		system "gunzip $gz1";
		waitpid $pid,0;
	}else{
		system "gunzip $gz2";
		exit(0);
	}
	$gz1=~/^(.*)\.gz$/;
	my $fq1=$1;
	$gz2=~/^(.*)\.gz$/;
	my $fq2=$1;
	
	return $fq1,$fq2;
}
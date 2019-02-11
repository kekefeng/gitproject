#需要一个输入文件指定分组和fastq文件前缀
use 5.012;
open IN,"<","pheno_data.txt" or die $!;
readline IN;
my %fastq;
while (<IN>){
	chomp;
	my ($fastq_id,$group)=split /\t/;
	$fastq{$fastq_id}=$group;
}

#根据物种判断index和gtf文件位置
my $gtf="/mnt/data4/songyf/scl-gfp-sequencing/genome/Mus_musculus_Ensemble_90.gtf";

#mapping reads and Assemble transcripts for each sample:
mkdir "gtf";
mkdir "mapinfo";
foreach my $sample(keys %fastq){
	system "stringtie -p8 -G $gtf -o gtf/${sample}.gtf -l ${sample} bam/${sample}.bam";
	system "echo gtf/${sample}.gtf >> mergelist.txt";
}

#Merge transcripts from all samples:
system "stringtie --merge -p 8 -G $gtf -o gtf/merged.gtf mergelist.txt";

#Estimate transcript abundances and create table counts for Ballgown
foreach my $sample(keys %fastq){
	system "stringtie -e -B -A -G gtf/merged.gtf -o ${sample}/${sample}_final.gtf bam/${sample}.bam";
}

=cut
system "Rscript diff.R";
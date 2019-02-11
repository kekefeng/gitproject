/apps/gatk-4.0.4.0/gatk GenotypeGVCFs -R /mnt/data2/genome/hg19_refseq/hg19.fa -V $1 -O all.vcf
rm $1

#filter snp之后分成各个样本
mkdir snp
/apps/gatk-4.0.4.0/gatk SelectVariants -select-type SNP -V all.vcf -O snp.vcf
/apps/gatk-4.0.4.0/gatk VariantFiltration -V snp.vcf --filter-expression "QD < 2.0 || MQ < 40.0 || FS > 60.0 || SOR > 3.0 || MQRankSum < -12.5 || ReadPosRankSum < -8.0 || QUAL < 50" --filter-name "lowqual" -O snp/snp.filter.vcf
cd snp
grep -v "lowqual" snp.filter.vcf > snp_pass.vcf  #提取
perl /mnt/data4/songyf/annovar/convert2annovar.pl -format vcf4 --allsample --outfile seperate snp.pass.vcf
perl /mnt/data4/songyf/annovar/table_annovar.pl seperate.XCF.avinput /mnt/data4/songyf/annovar/humandb/ -buildver hg19 -out snpanno -remove -protocol refGene,snp138,1000g2015aug_all,1000g2015aug_afr,1000g2015aug_amr,1000g2015aug_eas,1000g2015aug_eur,1000g2015aug_sas,esp6500siv2_all -operation g,f,f,f,f,f,f,f,f -nastring . 
perl /mnt/data4/songyf/annovar/table_annovar.pl -protocol ljb26_all -operation f -build hg19 -out 


#处理indel
/apps/gatk-4.0.4.0/gatk SelectVariants -select-type INDEL -V all.vcf -O indel.vcf
/apps/gatk-4.0.4.0/gatk VariantFiltration -V indel.vcf --filter-expression "QD<2.0 || FS>200.0 ||SOR>10.0 || MQRankSum<-12.5 || ReadPosRankSum<-8.0 || QUAL < 50" --filter-name "lowqual" -O indel.filter.vcf
grep -v "lowqual" indel.filter.vcf > indel_pass.vcf


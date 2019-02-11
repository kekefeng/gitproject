library(ggplot2)
sample=c("HYF","XCF","XLY","XYZ","XN","XCL","XR")

for(i in sample){
	infile=paste0(i,".length")
	k=read.table(infile,header=T,sep="\t")
	max1=max(k$Genome)
	pngfile=paste0(i,"_indel_length.png")
	levels(k$INDEL.length)=c("1","2","3","4","5","6",">6")
	p=ggplot()+geom_line(data = k,aes(x =INDEL.length,y =Genome,group=1),colour="blue",size=1.5)+geom_line(data = k,aes(x =INDEL.length,y =rescale(Exonic,c(0,max1)),group=1),colour="red",size=1.5)+  scale_y_continuous(breaks=pretty_breaks(5),sec.axis = sec_axis( ~rescale(.,c(0,250)),name = "Exonic(Red)",labels=(0:5)*50))
	p
	ggsave(pngfile)
}


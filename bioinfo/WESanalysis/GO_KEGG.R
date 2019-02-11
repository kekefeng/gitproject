library(clusterProfiler)
cols=c("ENTREZID","SYMBOL")
sample=c("HYF","XCF","XLY","XYZ","XN","XR","XCL")
for(i in sample){
	file=paste0(i,"_for_go_snp.txt")
	gene=read.table(file)
	gene=unique(sort(unlist(gene)))
	geneid=AnnotationDbi::select(org.Hs.eg.db,keys=gs,columns=cols,keytype="SYMBOL")$ENTREZID
	
	ego_BP	<- enrichGO(gene=geneid,'org.Hs.eg.db',ont="BP",pvalueCutoff=0.01,readable=TRUE)
	des_BP=head(ego_BP$Description,15)
	pvalue_BP=-log(head(ego_BP$p.adjust,15))
	filename=paste0(i,"_BP.out")
	write.table(as.data.frame(ego_BP),filename,row.names=F,sep="\t")
	
	ego_CC	<- enrichGO(gene=geneid,'org.Hs.eg.db',ont="CC",pvalueCutoff=0.01,readable=TRUE)
	des_CC=head(ego_CC$Description,15)
	pvalue_CC=-log(head(ego_CC$p.adjust,15))
	filename=paste0(i,"_CC.out")
	write.table(as.data.frame(ego_CC),filename,row.names=F,sep="\t")	
	
	ego_MF	<- enrichGO(gene=geneid,'org.Hs.eg.db',ont="MF",pvalueCutoff=0.01,readable=TRUE)
	des_MF=head(ego_MF$Description,15)
	pvalue_MF=-log(head(ego_MF$p.adjust,15))	
	filename=paste0(i,"_MF.out")
	write.table(as.data.frame(ego_MF),filename,row.names=F,sep="\t")
	
	des3=c(des_BP,des_CC,des_MF)
	pvalue3=c(pvalue_BP,pvalue_CC,pvalue_MF)
	catelogy=c(rep("biological_process",15),rep("cellular_component",15),rep("molecular_function",15))	
	dt=data.frame(des=des3,pvalue=pvalue3,catelogy=catelogy)

	pdffile=paste0(i,"_GO.pdf")
	GO_term=reorder(dt$des,-dt$pvalue)
	"-log(p.adjust)"=dt$pvalue
	p  <- ggplot(data=dt, aes(x=GO_term, y=`-log(p.adjust)`))+geom_bar(stat="identity",aes(fill=catelogy))+facet_wrap(~catelogy, scales = "free_x")+theme(axis.text.x=element_text(angle=90,hjust=1,size=15,vjust=0.25,colour="green"))
	ggsave(pdffile,width=20,height=10.4)
	
	keggout=enrichKEGG(geneid,organism = "hsa",keyType="kegg",pvalueCutoff = 0.05,pAdjustMethod = "BH")
	filename=paste0(i,"_KEGG.out")
	write.table(as.data.frame(keggout),filename,row.names=F,sep="\t")
	
	pdffile=paste0(i,"_KEGG.pdf")
	des_KEGG=head(keggout$Description,15)
	pvalue_KEGG=-log(head(keggout$p.adjust,15))
	d_kegg=data.frame(des=des_KEGG,pvalue=pvalue_KEGG)
	KEGG_term=reorder(d_kegg$des,-d_kegg$pvalue)
	"-log(p.adjust)"=d_kegg$pvalue
	p=ggplot(data=d_kegg,aes(x=KEGG_term, y=`-log(p.adjust)`))+geom_bar(stat="identity",fill="green")+theme(axis.text.x=element_text(angle=90,hjust=1,size=15,vjust=0.25,colour="black"))
	ggsave(pdffile)
}
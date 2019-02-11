sample=c("GFP_Ctrl","GFP_D11","GFP_D19","VCR_D8","VCR_D13")
genelist<-numeric()
alllist<-list()
for(i in sample){
  filename=paste0(i,".diff")
  dat=read.table(filename,skip=10,sep="\t")
  dat.sel=dat[,3]
  names(dat.sel)=dat[,2]
  logi=which(dat.sel==0)
  dat.sel.nonzero=dat.sel[-logi]
  len=length(dat.sel.nonzero)
  num=floor(len*0.025)
  sort.dat=sort(dat.sel.nonzero)
  lowest=names(sort.dat[1:num])
  highest=names(tail(sort.dat,num))
  locallist=c(lowest,highest)
  a=data.frame()
#  for(j in locallist){
#    tmp=filter(dat,V2==j)
#    a=rbind(a,tmp)
#  }
  write.table(locallist,i,quote=F,row.names=F)
}
totalname=names(alllist)

#pic1
pic=ggplot(data=k,mapping=aes(x=Sample,y=Percentage,fill=type)) +geom_bar(stat='identity',position='fill') + theme(axis.text.x = element_text(size = 15, vjust = 0.5, hjust = 0.5, angle = 45))  + theme(axis.text.y=element_text(size=15)) + theme(legend.text = element_text(size=15))
##
#hjust = 1 水平对齐
#vjust = 0-1 0在最下面，1最靠近上面
#axis.title.x   lengend.title

#双坐标轴indel length
levels(k$INDEL.length)=c("1","2","3","4","5","6",">6")

max1=max(k$Genome)
p=ggplot()+geom_line(data = k,aes(x =INDEL.length,y =Genome,group=1),colour="blue",size=1.5)+geom_line(data = k,aes(x =INDEL.length,y =rescale(Exonic,c(0,max1)),group=1),colour="red",size=1.5)+  scale_y_continuous(breaks=pretty_breaks(5),sec.axis = sec_axis( ~rescale(.,c(0,250)),name = "Exonic(Red)",labels=(0:5)*50))

options(scipen=200)  #取消科学计数法
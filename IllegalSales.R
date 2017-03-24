rm(list = ls(all=T))
gc()
library(data.table)
datatotal<-fread("D:\\�����ļ�\\�ͼ۲�ѯ\\201702\\201702���߷ָ�A.txt",sep = "|",header = TRUE)

#load(file = "D:\\�����ļ�\\�ͼ۲�ѯ\\201701\\201701djdata.RData")




head(datatotal,1)
pinlei<-'����'
qulingbiao<-datatotal[��������>0&�������=='��ͨ����'&ʮ��Ʒ��==pinlei&!grepl("���",����Ʒ��),.(�ֲ�,��Ʒ����,��Ʒ����,��Ʊ����,��������,���۽��,�ۿ۽��)]

i<-1

bjdate<-as.Date(paste("2017-","1-",i,sep=""))
zhongjianzhi<-qulingbiao[abs(difftime(bjdate,��Ʊ����,units = 'days'))<4,.(�������=bjdate,����������=sum(��������),�����۽��=sum(���۽��),���ۿ۽��=sum(�ۿ۽��)),by=.(�ֲ�,��Ʒ����,��Ʒ����)]
fenbujiage11<-zhongjianzhi
i<-i+1

while(i<32){
  bjdate<-as.Date(paste("2017-","2-",i,sep=""))
  zhongjianzhi<-qulingbiao[abs(difftime(bjdate,��Ʊ����,units = 'days'))<4,.(�������=bjdate,����������=sum(��������),�����۽��=sum(���۽��),���ۿ۽��=sum(�ۿ۽��)),by=.(�ֲ�,��Ʒ����,��Ʒ����)]
  #fenbujiage11<-zhongjianzhi
  #fenbujiage11<-fenbujiage12
  fenbujiage11<-funion(fenbujiage11, zhongjianzhi, all=TRUE)
  i<-i+1
}

head(fenbujiage11)
qulingbiao2<-datatotal[��������>0&�������=='��ͨ����'&ʮ��Ʒ��==pinlei&!grepl("���",����Ʒ��),]
qulingbiao2<-merge.data.frame(qulingbiao2,kehuzl2,by = c('�ֲ�','�ͻ�����','�ͻ���ַ','�ͻ��ֻ�','�ͻ��绰','��Ա����'),all.x = T)
qulingbiao2<-as.data.table(qulingbiao2)

# head(qulingbiao2)
kehuhuizong1<-qulingbiao2[,.(��������1=sum(��������),���۽��1=sum(���۽��),�ۿ۽��1=sum(�ۿ۽��)),by=.(�ֲ�,��Ʒ����,��Ʒ����,��Ʊ����,bj2)]
kehuhuizong1[,':='(�������=format(as.Date(��Ʊ����),'%Y-%m-%d'))]
fenbujiage11[,':='(�������=format(as.Date(�������),'%Y-%m-%d'))]
# is.data.table(kehuhuizong1)
# is.data.table(fenbujiage11)
# head(kehuhuizong1)
# head(fenbujiage11)

kehuhuizong2<-merge.data.frame(kehuhuizong1,fenbujiage11,by=c("�ֲ�","��Ʒ����","��Ʒ����","�������"),all.x = T)
# is.data.table(kehuhuizong2)
kehuhuizong2<-as.data.table(kehuhuizong2)

kehuhuizong2[,':='(�ɽ���=(���۽��1-�ۿ۽��1)/��������1,�ֲ��ɽ���=(�����۽��-���۽��1-���ۿ۽��+�ۿ۽��1)/(����������-��������1))]

kehuhuizong2[����������==��������1,':='(�ɽ���=(���۽��1-�ۿ۽��1)/��������1,�ֲ��ɽ���=(�����۽��-���ۿ۽��)/����������)]

# kehuhuizong2[�ֲ��ɽ���==NaN,]
# kehuhuizong2[����������==��������1,�ֲ��ɽ���]

kehuhuizong2[,':='(���=�ֲ��ɽ���-�ɽ���)]
kehuhuizong2[,':='(���=���*��������1)]

# chaehuizong1<-kehuhuizong2[���>0,.(�ܲ��=sum(���),���ۿ�=sum(�ۿ۽��1),�����۽��=sum(���۽��1),����������=sum(��������1)),by=.(�ֲ�,�ͻ�����,��Ա����)]
# chaehuizong2<-kehuhuizong2[���>0,.(�ܲ��=sum(���),���ۿ�=sum(�ۿ۽��1),�����۽��=sum(���۽��1),����������=sum(��������1)),by=.(�ֲ�,�ͻ�����)]
chaehuizong3<-kehuhuizong2[���>30,.(�ܲ��=sum(���),���ۿ�=sum(�ۿ۽��1),�����۽��=sum(���۽��1),����������=sum(��������1)),by=.(bj2)]
# chaehuizongys3<-chaehuizong3[�ܲ��>2000&����������>10,]

chaehuizong3<-merge.data.frame(chaehuizong3,jishu,by = c('bj2'),all.x = T)

# write.csv(chaehuizongys3,file = "D:\\�����ļ�\\�ͼ۲�ѯ\\201702\\chaehuizongys3.csv")
write.csv(kehuhuizong2,file = "D:\\�����ļ�\\�ͼ۲�ѯ\\201702\\kehuhuizong2.csv")
write.csv(chaehuizong3,file = "D:\\�����ļ�\\�ͼ۲�ѯ\\201702\\chaehuizong3.csv")
# write.csv(kehuzl2,file = "D:\\�����ļ�\\�ͼ۲�ѯ\\201702\\kehuzl2.csv")



setorder(chaehuizongys3,-�ܲ��)
view(chaehuizongys3)
head(kehuhuizong2)



chaehuizongys1<-chaehuizong1[�ܲ��>5000,]
chaehuizongys2<-chaehuizong2[�ܲ��>5000,]
djkehu<-chaehuizongys2[,,by=.(�ֲ�,�ͻ�����)]
djkehu[,':='(�ֲ��ͻ�����=paste(�ֲ�,�ͻ�����,sep=''),���='1')]
djbiaoka<-djkehu[,.(�ֲ��ͻ�����,���)]
djbiaoka
djbiaoka<-as.data.table(djbiaoka)
kehuhuizong2[,':='(�ֲ��ͻ�����=as.character(paste(�ֲ�,�ͻ�����,sep='')))]


mingxi20<-merge.data.frame(kehuhuizong2,djbiaoka,by=c("�ֲ��ͻ�����"),all.x = T)
mingxi20<-as.data.table(mingxi20)
mingxi2<-mingxi20[���=='1',]

write.csv(mingxi2,file = "D:\\�����ļ�\\�ͼ۲�ѯ\\201701\\mingxi2.csv")


write.csv(chaehuizongys1,file = "D:\\�����ļ�\\�ͼ۲�ѯ\\201701\\chaehuizongys1.csv")
write.csv(chaehuizongys2,file = "D:\\�����ļ�\\�ͼ۲�ѯ\\201701\\chaehuizongys2.csv")
write.csv(chaehuizongys2,file = "D:\\�����ļ�\\�ͼ۲�ѯ\\201701\\chaehuizongys2.csv")


save(datatotal,fenbujiage11,kehuhuizong2,chaehuizongys1,chaehuizongys2,chaehuizongys3,file = "D:\\�����ļ�\\�ͼ۲�ѯ\\201701\\201701djdata-v2.RData")
save(datatotal,file = "D:\\�����ļ�\\�ͼ۲�ѯ\\201702\\201702.RData")

rm(list = ls(all=T))
rm(h,zhongjianzhi)
gc()
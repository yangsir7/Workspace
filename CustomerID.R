library(data.table)

rm(list = ls(all=T))
gc()
load(file = "D:\\�����ļ�\\�ͼ۲�ѯ\\201701\\201701djdata.RData")
load(file = "D:\\�����ļ�\\�ͼ۲�ѯ\\201701\\201701djdata-v2.RData")
load(file = "D:\\�����ļ�\\�ͼ۲�ѯ\\201702\\201702.RData")
load(file = "J:\\201701djdata-v2.RData")
load(file = "J:\\kehuzl.RData")
save(datatotal,biaojihz,kehuzl2,file = "J:\\kehuzl.RData")
save(datatotal,file = "J:\\kehuzl.RData")

pinlei<-'�ʵ�'
kehuzl<-datatotal[��������>0&�������=='��ͨ����'&ʮ��Ʒ��==pinlei&!grepl("���",����Ʒ��),.(�ֲ�,�ͻ�����,�ͻ���ַ,�ͻ��ֻ�,�ͻ��绰,��Ա����)]


kehuzl[,id:=1:nrow(kehuzl)]
kehuzl2<-kehuzl[!duplicated(kehuzl[,1:6]),]
fenbu<-kehuzl2[,.N,by=.(�ֲ�)]
biaojihz<-kehuzl2[FALSE,][,bj:=as.numeric()]
fh<-list()
for (v in 1:nrow(fenbu)) {
  fh[v]<-as.character(fenbu[v,1])
}

kehu0<-kehuzl2[,]
kehu0[nchar(�ͻ���ַ)<4,�ͻ���ַ:=as.character(id)]
kehu0[nchar(�ͻ��ֻ�)<6,�ͻ��ֻ�:=as.character(id)]
kehu0[nchar(�ͻ��绰)<6,�ͻ��绰:=as.character(id)]
kehu0[nchar(��Ա����)<11,��Ա����:=as.character(id)]


for (k in fh) {
  gc()
  kehu1<-kehu0[grepl(k,�ֲ�),]
  i<-0
  biaoji<-kehu1[FALSE,][,bj:=as.numeric()]
  while (nrow(kehu1)>0) {
    i<-i+1
    tj<-as.list(as.character(kehu1[1,2:6]))
    luoji<-kehu1[,(�ͻ�����==tj[1]|�ͻ���ַ==tj[2]|�ͻ��ֻ�==tj[3]|�ͻ��绰==tj[4]|��Ա����==tj[5])]
    biaoji2<-kehu1[luoji,];biaoji2[,bj:=i]
    kehu1<-kehu1[!luoji,]
    j<-1
    while (j<nrow(biaoji2)) {
      j<-j+1
      tj<-as.list(as.character(biaoji2[j,2:6]))
      luoji<-kehu1[,(�ͻ�����==tj[1]|�ͻ���ַ==tj[2]|�ͻ��ֻ�==tj[3]|�ͻ��绰==tj[4]|��Ա����==tj[5])]
      biaoji3<-kehu1[luoji,];biaoji3[,bj:=i]
      biaoji2<-funion(biaoji2,biaoji3,all = T)
      kehu1<-kehu1[!luoji,]
    }
    biaoji<-funion(biaoji,biaoji2,all = T)
  }
  biaojihz<-funion(biaojihz,biaoji,all = T)
}

biaojihz2<-biaojihz[,.(id,bj2=paste(�ֲ�,bj,sep = ''))]
kehuzl2<-kehuzl[!duplicated(kehuzl[,1:6]),]
kehuzl2<-merge(kehuzl2,biaojihz2,by = c('id'),all.x = T)

kehuzl<-merge.data.frame(kehuzl,kehuzl2,by = c('�ֲ�','�ͻ�����','�ͻ���ַ','�ͻ��ֻ�','�ͻ��绰','��Ա����'),all.x = T)
is.data.table(kehuzl)


jishu<-kehuzl2[,.N,by=.(bj2)]
setorder(jishu,-N)
View(jishu)

guancha<-merge.data.frame(kehuzl2,jishu,by = c('bj2'),all.x = T)
View(guancha)
write.csv(guancha,file = "D:\\�����ļ�\\�ͼ۲�ѯ\\201702\\guancha.csv")
write.csv(guancha,file = "D:\\�����ļ�\\�ͼ۲�ѯ\\201701\\guancha.csv")

youxiaobi<-jishu[N>2&N<25,bj]
luoji<-biaoji[,bj %in% youxiaobi]
guancha<-biaoji[luoji,]
setkey(guancha,bj)
library(data.table)

rm(list = ls(all=T))
gc()
load(file = "D:\\工作文件\\低价查询\\201701\\201701djdata.RData")
load(file = "D:\\工作文件\\低价查询\\201701\\201701djdata-v2.RData")
load(file = "D:\\工作文件\\低价查询\\201702\\201702.RData")
load(file = "J:\\201701djdata-v2.RData")
load(file = "J:\\kehuzl.RData")
save(datatotal,biaojihz,kehuzl2,file = "J:\\kehuzl.RData")
save(datatotal,file = "J:\\kehuzl.RData")

pinlei<-'彩电'
kehuzl<-datatotal[销售数量>0&销售类别=='普通销售'&十大品类==pinlei&!grepl("配件",二级品类),.(分部,客户名称,客户地址,客户手机,客户电话,会员卡号)]


kehuzl[,id:=1:nrow(kehuzl)]
kehuzl2<-kehuzl[!duplicated(kehuzl[,1:6]),]
fenbu<-kehuzl2[,.N,by=.(分部)]
biaojihz<-kehuzl2[FALSE,][,bj:=as.numeric()]
fh<-list()
for (v in 1:nrow(fenbu)) {
  fh[v]<-as.character(fenbu[v,1])
}

kehu0<-kehuzl2[,]
kehu0[nchar(客户地址)<4,客户地址:=as.character(id)]
kehu0[nchar(客户手机)<6,客户手机:=as.character(id)]
kehu0[nchar(客户电话)<6,客户电话:=as.character(id)]
kehu0[nchar(会员卡号)<11,会员卡号:=as.character(id)]


for (k in fh) {
  gc()
  kehu1<-kehu0[grepl(k,分部),]
  i<-0
  biaoji<-kehu1[FALSE,][,bj:=as.numeric()]
  while (nrow(kehu1)>0) {
    i<-i+1
    tj<-as.list(as.character(kehu1[1,2:6]))
    luoji<-kehu1[,(客户名称==tj[1]|客户地址==tj[2]|客户手机==tj[3]|客户电话==tj[4]|会员卡号==tj[5])]
    biaoji2<-kehu1[luoji,];biaoji2[,bj:=i]
    kehu1<-kehu1[!luoji,]
    j<-1
    while (j<nrow(biaoji2)) {
      j<-j+1
      tj<-as.list(as.character(biaoji2[j,2:6]))
      luoji<-kehu1[,(客户名称==tj[1]|客户地址==tj[2]|客户手机==tj[3]|客户电话==tj[4]|会员卡号==tj[5])]
      biaoji3<-kehu1[luoji,];biaoji3[,bj:=i]
      biaoji2<-funion(biaoji2,biaoji3,all = T)
      kehu1<-kehu1[!luoji,]
    }
    biaoji<-funion(biaoji,biaoji2,all = T)
  }
  biaojihz<-funion(biaojihz,biaoji,all = T)
}

biaojihz2<-biaojihz[,.(id,bj2=paste(分部,bj,sep = ''))]
kehuzl2<-kehuzl[!duplicated(kehuzl[,1:6]),]
kehuzl2<-merge(kehuzl2,biaojihz2,by = c('id'),all.x = T)

kehuzl<-merge.data.frame(kehuzl,kehuzl2,by = c('分部','客户名称','客户地址','客户手机','客户电话','会员卡号'),all.x = T)
is.data.table(kehuzl)


jishu<-kehuzl2[,.N,by=.(bj2)]
setorder(jishu,-N)
View(jishu)

guancha<-merge.data.frame(kehuzl2,jishu,by = c('bj2'),all.x = T)
View(guancha)
write.csv(guancha,file = "D:\\工作文件\\低价查询\\201702\\guancha.csv")
write.csv(guancha,file = "D:\\工作文件\\低价查询\\201701\\guancha.csv")

youxiaobi<-jishu[N>2&N<25,bj]
luoji<-biaoji[,bj %in% youxiaobi]
guancha<-biaoji[luoji,]
setkey(guancha,bj)
rm(list = ls(all=T))
gc()
library(data.table)
datatotal<-fread("D:\\工作文件\\低价查询\\201702\\201702竖线分割A.txt",sep = "|",header = TRUE)

#load(file = "D:\\工作文件\\低价查询\\201701\\201701djdata.RData")




head(datatotal,1)
pinlei<-'电脑'
qulingbiao<-datatotal[销售数量>0&销售类别=='普通销售'&十大品类==pinlei&!grepl("配件",二级品类),.(分部,商品编码,商品名称,开票日期,销售数量,销售金额,折扣金额)]

i<-1

bjdate<-as.Date(paste("2017-","1-",i,sep=""))
zhongjianzhi<-qulingbiao[abs(difftime(bjdate,开票日期,units = 'days'))<4,.(标记日期=bjdate,周销售数量=sum(销售数量),周销售金额=sum(销售金额),周折扣金额=sum(折扣金额)),by=.(分部,商品编码,商品名称)]
fenbujiage11<-zhongjianzhi
i<-i+1

while(i<32){
  bjdate<-as.Date(paste("2017-","2-",i,sep=""))
  zhongjianzhi<-qulingbiao[abs(difftime(bjdate,开票日期,units = 'days'))<4,.(标记日期=bjdate,周销售数量=sum(销售数量),周销售金额=sum(销售金额),周折扣金额=sum(折扣金额)),by=.(分部,商品编码,商品名称)]
  #fenbujiage11<-zhongjianzhi
  #fenbujiage11<-fenbujiage12
  fenbujiage11<-funion(fenbujiage11, zhongjianzhi, all=TRUE)
  i<-i+1
}

head(fenbujiage11)
qulingbiao2<-datatotal[销售数量>0&销售类别=='普通销售'&十大品类==pinlei&!grepl("配件",二级品类),]
qulingbiao2<-merge.data.frame(qulingbiao2,kehuzl2,by = c('分部','客户名称','客户地址','客户手机','客户电话','会员卡号'),all.x = T)
qulingbiao2<-as.data.table(qulingbiao2)

# head(qulingbiao2)
kehuhuizong1<-qulingbiao2[,.(销售数量1=sum(销售数量),销售金额1=sum(销售金额),折扣金额1=sum(折扣金额)),by=.(分部,商品编码,商品名称,开票日期,bj2)]
kehuhuizong1[,':='(标记日期=format(as.Date(开票日期),'%Y-%m-%d'))]
fenbujiage11[,':='(标记日期=format(as.Date(标记日期),'%Y-%m-%d'))]
# is.data.table(kehuhuizong1)
# is.data.table(fenbujiage11)
# head(kehuhuizong1)
# head(fenbujiage11)

kehuhuizong2<-merge.data.frame(kehuhuizong1,fenbujiage11,by=c("分部","商品编码","商品名称","标记日期"),all.x = T)
# is.data.table(kehuhuizong2)
kehuhuizong2<-as.data.table(kehuhuizong2)

kehuhuizong2[,':='(成交价=(销售金额1-折扣金额1)/销售数量1,分部成交价=(周销售金额-销售金额1-周折扣金额+折扣金额1)/(周销售数量-销售数量1))]

kehuhuizong2[周销售数量==销售数量1,':='(成交价=(销售金额1-折扣金额1)/销售数量1,分部成交价=(周销售金额-周折扣金额)/周销售数量)]

# kehuhuizong2[分部成交价==NaN,]
# kehuhuizong2[周销售数量==销售数量1,分部成交价]

kehuhuizong2[,':='(差价=分部成交价-成交价)]
kehuhuizong2[,':='(差额=差价*销售数量1)]

# chaehuizong1<-kehuhuizong2[差价>0,.(总差额=sum(差额),总折扣=sum(折扣金额1),总销售金额=sum(销售金额1),总销售数量=sum(销售数量1)),by=.(分部,客户名称,会员卡号)]
# chaehuizong2<-kehuhuizong2[差价>0,.(总差额=sum(差额),总折扣=sum(折扣金额1),总销售金额=sum(销售金额1),总销售数量=sum(销售数量1)),by=.(分部,客户名称)]
chaehuizong3<-kehuhuizong2[差价>30,.(总差额=sum(差额),总折扣=sum(折扣金额1),总销售金额=sum(销售金额1),总销售数量=sum(销售数量1)),by=.(bj2)]
# chaehuizongys3<-chaehuizong3[总差额>2000&总销售数量>10,]

chaehuizong3<-merge.data.frame(chaehuizong3,jishu,by = c('bj2'),all.x = T)

# write.csv(chaehuizongys3,file = "D:\\工作文件\\低价查询\\201702\\chaehuizongys3.csv")
write.csv(kehuhuizong2,file = "D:\\工作文件\\低价查询\\201702\\kehuhuizong2.csv")
write.csv(chaehuizong3,file = "D:\\工作文件\\低价查询\\201702\\chaehuizong3.csv")
# write.csv(kehuzl2,file = "D:\\工作文件\\低价查询\\201702\\kehuzl2.csv")



setorder(chaehuizongys3,-总差额)
view(chaehuizongys3)
head(kehuhuizong2)



chaehuizongys1<-chaehuizong1[总差额>5000,]
chaehuizongys2<-chaehuizong2[总差额>5000,]
djkehu<-chaehuizongys2[,,by=.(分部,客户名称)]
djkehu[,':='(分部客户名称=paste(分部,客户名称,sep=''),标记='1')]
djbiaoka<-djkehu[,.(分部客户名称,标记)]
djbiaoka
djbiaoka<-as.data.table(djbiaoka)
kehuhuizong2[,':='(分部客户名称=as.character(paste(分部,客户名称,sep='')))]


mingxi20<-merge.data.frame(kehuhuizong2,djbiaoka,by=c("分部客户名称"),all.x = T)
mingxi20<-as.data.table(mingxi20)
mingxi2<-mingxi20[标记=='1',]

write.csv(mingxi2,file = "D:\\工作文件\\低价查询\\201701\\mingxi2.csv")


write.csv(chaehuizongys1,file = "D:\\工作文件\\低价查询\\201701\\chaehuizongys1.csv")
write.csv(chaehuizongys2,file = "D:\\工作文件\\低价查询\\201701\\chaehuizongys2.csv")
write.csv(chaehuizongys2,file = "D:\\工作文件\\低价查询\\201701\\chaehuizongys2.csv")


save(datatotal,fenbujiage11,kehuhuizong2,chaehuizongys1,chaehuizongys2,chaehuizongys3,file = "D:\\工作文件\\低价查询\\201701\\201701djdata-v2.RData")
save(datatotal,file = "D:\\工作文件\\低价查询\\201702\\201702.RData")

rm(list = ls(all=T))
rm(h,zhongjianzhi)
gc()

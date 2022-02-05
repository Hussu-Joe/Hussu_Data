#c<-read.csv("c:/Users/hussu/Desktop/4주차/weatherf.csv")
#c<-c$x
#c<-NULL
#tt<-acf(mstime$X106320,lag.max = 8)
#round(max(tt$acf[2:8]),1)
#mstime$X112469

#c<-colnames(mstime[,c(2:4829)])
for (i in 1:21204) {
  master$상품코드[i]<-paste0("X",master$상품코드[i])
}

length(intersect(colnames(mstime),master2$상품코드))
t<-rownames(master)[master$위탁구분=="일반"]
master2<-master[t,]
rownames(master2)<-seq(1,9631,1)
in2<-NULL
for (i in 1:9631) {
  p<-substr(master2$상품명[i],1,1)
  in2<-append(in2,p)
}
unique(in2)

master2$in2<-in2
p<-rownames(master2)[master2$in2=="("|master2$in2=="["]
master3<-master2[p,]
rownames(master3)<-seq(1,1822,1)
p<-grep("삭제",master3$상품명)
master3<-master3[-p,]
write.table(master3,"c:/Users/hussu/Desktop/4주차/일반인데전용.csv",sep=",")


master2$상품명
write.table(master2,"c:/Users/hussu/Desktop/4주차/사용중지.csv",sep=",")
print("U+005B")

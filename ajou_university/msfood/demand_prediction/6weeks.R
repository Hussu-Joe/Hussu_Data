library(forecast)

mstime<-read.csv("c:/Users/hussu/Desktop/6주차/mstime2.csv")
mstime$X<-as.Date(mstime$X)
rownames(mstime)<-mstime$X
mstime<-mstime[,-1]
stat104<-read.csv("c:/Users/hussu/Desktop/6주차/기초통계.csv")
orderf<-read.csv("c:/Users/hussu/Desktop/6주차/orderf.csv")
item_code<-colnames(mstime)
colnames(orderf)<-item_code
#### 6weeks 01

fact<-NULL
for (i in item_code){
  t<-as.factor(mstime[[i]])
  s<-summary(t)
  l<-length(s)
  fact<-append(fact,l)
}
write.table(stat104,"c:/Users/hussu/Desktop/6주차/statnew.csv",sep=",")
stat104$facto<-fact
#### 6weeks 02


library(ggplot2)
library(reshape2)

sum_ms_104<-read.csv("c:/Users/hussu/Desktop/4주차/기초통계.csv", stringsAsFactors=F)
sum_ms_52<-read.csv("c:/Users/hussu/Desktop/4주차/기초통계_52.csv", stringsAsFactors=F)
sum_ms_26<-read.csv("c:/Users/hussu/Desktop/4주차/기초통계_26.csv", stringsAsFactors=F)

zeroc<-data.frame(sum_ms_104$zeroc,sum_ms_52$zeroc,sum_ms_26$zeroc)
zeroc$sum_ms_104.zeroc<-sort(zeroc$sum_ms_104.zeroc,decreasing = T)
zeroc$sum_ms_52.zeroc[1:3431]<-sort(zeroc$sum_ms_52.zeroc[1:3431],decreasing = T)
zeroc$sum_ms_26.zeroc[1:3328]<-sort(zeroc$sum_ms_26.zeroc[1:3328],decreasing = T)
plot(zeroc$sum_ms_104.zeroc,type = "l")
plot(zeroc$sum_ms_52.zeroc,type = "l")
plot(zeroc$sum_ms_26.zeroc,type = "l")
index<-seq(1,3448,1)
zeroc<-melt(zeroc,"index")
p<-ggplot(zeroc,aes(x=index,y=value,colour=variable,group=variable))+geom_line()+ggtitle("zero비율의 비교")
p

sum_ms_104$acf_time<-as.factor(sum_ms_104$acf_time)
p<-ggplot(data = sum_ms_104,aes(acf_time))
p+geom_bar(aes(fill=acf_time))+ggtitle("104주차 자기상관시점")

sum_ms_104$temp2_time<-as.factor(sum_ms_104$temp2_time)
p<-ggplot(data = sum_ms_104,aes(temp2_time))
p+geom_bar(aes(fill=temp2_time))+ggtitle("104주차 SKU와 기온의 시차상관시점")

sum_ms_104$prec_time<-as.factor(sum_ms_104$prec_time)
p<-ggplot(data= sum_ms_104,aes(prec_time))
p+geom_bar(aes(fill=prec_time))+ggtitle("104주차 SKU와 강수량의 시차상관시점")

sum_ms_52$acf_time<-as.factor(sum_ms_52$acf_time)
p<-ggplot(data = sum_ms_52,aes(acf_time))
p+geom_bar(aes(fill=acf_time))+ggtitle("52주차 자기상관시점")

sum_ms_52$temp2_time<-as.factor(sum_ms_52$temp2_time)
p<-ggplot(data = sum_ms_52,aes(temp2_time))
p+geom_bar(aes(fill=temp2_time))+ggtitle("52주차 SKU와 기온의 시차상관시점")


sum_ms_52$prec_time<-as.factor(sum_ms_52$prec_time)
p<-ggplot(data= sum_ms_52,aes(prec_time))
p+geom_bar(aes(fill=prec_time))+ggtitle("52주차 SKU와 강수량의 시차상관시점")

sum_ms_26$acf_time<-as.factor(sum_ms_26$acf_time)
p<-ggplot(data = sum_ms_26,aes(acf_time))
p+geom_bar(aes(fill=acf_time))+ggtitle("26주차 자기상관시점")

sum_ms_26$temp2_time<-as.factor(sum_ms_26$temp2_time)
p<-ggplot(data = sum_ms_26,aes(temp2_time))
p+geom_bar(aes(fill=temp2_time))+ggtitle("26주차 SKU와 기온의 시차상관시점")


sum_ms_26$prec_time<-as.factor(sum_ms_26$prec_time)
p<-ggplot(data= sum_ms_26,aes(prec_time))
p+geom_bar(aes(fill=prec_time))+ggtitle("26주차 SKU와 강수량의 시차상관시점")

acf_final<-data.frame(index,sum_ms_104$acf_value,sum_ms_52$acf_value,sum_ms_26$acf_value)
acf_final$sum_ms_104.acf_value[!is.na(acf_final$sum_ms_104.acf_value)]<-sort(acf_final$sum_ms_104.acf_value[!is.na(acf_final$sum_ms_104.acf_value)])
acf_final$sum_ms_52.acf_value[!is.na(acf_final$sum_ms_52.acf_value)]<-sort(acf_final$sum_ms_52.acf_value[!is.na(acf_final$sum_ms_52.acf_value)])
acf_final$sum_ms_26.acf_value[!is.na(acf_final$sum_ms_26.acf_value)]<-sort(acf_final$sum_ms_26.acf_value[!is.na(acf_final$sum_ms_26.acf_value)])




acf_final<-melt(acf_final,"index")
p<-ggplot(acf_final,aes(x=index,y=value,colour=variable,group=variable))+geom_line()+ggtitle("자기상관계수 비교")
p

temp2_final<-data.frame(index,sum_ms_104$temp2_value,sum_ms_52$temp2_value,sum_ms_26$temp2_value)
temp2_final$sum_ms_104.temp2_value[!is.na(temp2_final$sum_ms_104.temp2_value)]<-sort(temp2_final$sum_ms_104.temp2_value[!is.na(temp2_final$sum_ms_104.temp2_value)])
temp2_final$sum_ms_52.temp2_value[!is.na(temp2_final$sum_ms_52.temp2_value)]<-sort(temp2_final$sum_ms_52.temp2_value[!is.na(temp2_final$sum_ms_52.temp2_value)])
temp2_final$sum_ms_26.temp2_value[!is.na(temp2_final$sum_ms_26.temp2_value)]<-sort(temp2_final$sum_ms_26.temp2_value[!is.na(temp2_final$sum_ms_26.temp2_value)])


temp2_final<-melt(temp2_final,"index")
p<-ggplot(temp2_final,aes(x=index,y=value,colour=variable,group=variable))+geom_line()+ggtitle("기온 시차상관계수 비교")
p


prec_final<-data.frame(index, sum_ms_104$prec_value,sum_ms_52$prec_value,sum_ms_26$prec_value)
prec_final$sum_ms_104.prec_value[!is.na(prec_final$sum_ms_104.prec_value)]<-sort(prec_final$sum_ms_104.prec_value[!is.na(prec_final$sum_ms_104.prec_value)])
prec_final$sum_ms_52.prec_value[!is.na(prec_final$sum_ms_52.prec_value)]<-sort(prec_final$sum_ms_52.prec_value[!is.na(prec_final$sum_ms_52.prec_value)])
prec_final$sum_ms_26.prec_value[!is.na(prec_final$sum_ms_26.prec_value)]<-sort(prec_final$sum_ms_26.prec_value[!is.na(prec_final$sum_ms_26.prec_value)])

prec_final<-melt(prec_final,"index")
p<-ggplot(prec_final,aes(x=index,y=value,colour=variable,group=variable))+geom_line()+ggtitle("강수량 시차상관계수 비교")
p

sum_ms_104$sample_c<-as.factor(sum_ms_104$sample_c)
p<-ggplot(data= sum_ms_104,aes(sample_c))
p+geom_bar()+ggtitle("샘플갯수")

sum_ms_104$cv<-sum_ms_104$std/sum_ms_104$mean

sum_ms_52$cv<-sum_ms_52$std/sum_ms_52$mean
sum_ms_26$cv<-sum_ms_26$std/sum_ms_26$mean

cv_final<-data.frame(index,sum_ms_104$cv,sum_ms_52$cv,sum_ms_26$cv)
cv_final$sum_ms_104.cv[!is.na(cv_final$sum_ms_104.cv)]<-sort(cv_final$sum_ms_104.cv[!is.na(cv_final$sum_ms_104.cv)],decreasing = T)
cv_final$sum_ms_52.cv[!is.na(cv_final$sum_ms_52.cv)]<-sort(cv_final$sum_ms_52.cv[!is.na(cv_final$sum_ms_52.cv)],decreasing = T)
cv_final$sum_ms_26.cv[!is.na(cv_final$sum_ms_26.cv)]<-sort(cv_final$sum_ms_26.cv[!is.na(cv_final$sum_ms_26.cv)],decreasing = T)

cv_final<-melt(cv_final,"index")
p<-ggplot(cv_final,aes(x=index,y=value,colour=variable,group=variable))+geom_line()+ggtitle("변동계수 비교")
p


#080703













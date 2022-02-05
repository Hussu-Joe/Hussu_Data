library(reshape2)
library(ggplot2)

#weather<-read.csv2("c:/Users/hussu/Desktop/final/weather2.csv",sep=",",fileEncoding = "utf-8",stringsAsFactors = F)
#weather$temp<-as.numeric(weather$temp)
#weather$prec<-as.numeric(weather$prec)
#weather$humi<-as.numeric(weather$humi)
#for(i in 1:17746){
#  master$상품코드[i]<-paste0("X",master$상품코드[i])
#}

mstime2<-mstime[,intersect(colnames(mstime),subset(master,master$위탁구분=="4"&master$재고구분=="저장품")$상품코드)]
mstime3<-data.frame(mstime$Date,mstime2$X1075,mstime2$X107573,weather)
colnames(mstime3)[1:3]<-c("Date","X107566","X107573")
mstime3<-melt(mstime3,"Date")
mstime3$Date<-as.Date(mstime3$Date)
p<-ggplot(mstime3,aes(x=Date,y=value,colour=variable,group=variable))+geom_line()+ggtitle("4_저장품")
p

master$상품명[master$상품코드=="X107573"]

thy<-read.csv("./thy_FU.csv")
thy<-thy[,-1]
length(thy)
forbox<-function(x){
  value<-NULL
  times<-rep(1:35,length.out=415625)
  for (i in 1:11875)){
    value<-append(value,as.numeric(x[i,]))
  }
  df1<-data.frame(times,value)
  return(df1)
}








TG1<-forbox(thy[,1:35])

library(ggplot2)
length(rep(1:35,length.out=415625))
ggplot(TG1,aes(x=factor(times),y=value))+geom_boxplot()+ggtitle("TGbox")
ggsave("./TGboxnew.png",dpi = 400)
ggplot(TG1,aes(x=factor(times),y=value))+ylim(c(0,50))+geom_boxplot(outlier.shape = NA)+ggtitle("TGboxout")
ggsave("./TGboxoutnew.png",dpi = 400)
summary(thy$TG1)
summary(thy$TG2)
rep(1:35,len=length(x[,1]))
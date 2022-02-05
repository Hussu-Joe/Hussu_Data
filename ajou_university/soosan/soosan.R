install.packages("rcompanion")
library(ggplot2)
library(rcompanion)

soosan<-read.csv("c:/Users/hussu/Desktop/soosan/soosan1.csv")
soosan$SCR<-as.factor(soosan$SCR)

soosan1<-subset(soosan,soosan$SCR==1)
soosan2<-subset(soosan,soosan$SCR==0)
ttt<-data.frame(summary(soosan2,digits = 8))
ttt<-ttt[,-1]
library(tidyr)
ttt<-separate(ttt,Freq,c("stat1","value"),sep = ":")
ttt$stat1<-as.factor(ttt$stat1)
ttt$value<-as.numeric(ttt$value,digits=8)
ttt<-spread(ttt,stat1,value)
write.table(ttt,file = "c:/Users/hussu/Desktop/soosan/soosanstat0.csv",sep = ",")

ggplot(soosan,aes(x= CylDaeMax, y=CylDaeMin,colour=as.factor(SCR)))+geom_point()+ggtitle("대경부")+ggsave("./")

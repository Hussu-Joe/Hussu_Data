SB151<-read.csv("c:/Users/hussu/Desktop/soosan/soosan1.csv")
SB151$SCR<-as.factor(SB151$SCR)
SB81 <- read.csv("c:/Users/hussu/Desktop/soosan/SB81.csv")
SB81A<- read.csv("c:/Users/hussu/Desktop/soosan/SB81A.csv")
SB121<- read.csv("c:/Users/hussu/Desktop/soosan/SB121.csv")
SB140<- read.csv("c:/Users/hussu/Desktop/soosan/SB140.csv")
SB100<- read.csv("c:/Users/hussu/Desktop/soosan/SB100.csv")
SB140p<- read.csv("c:/Users/hussu/Desktop/soosan/SB140p.csv")
colnames(SB81)[1]<-"ID"
colnames(SB81A)[1]<-"ID"
colnames(SB121)[1]<-"ID"
colnames(SB140)[1]<-"ID"
colnames(SB100)[1]<-"ID"
colnames(SB140p)[1]<-"ID"
SB140p$SCR[is.na(SB140p$SCR)]=0
#### 20190225
library(ggplot2)
library(plotly)
theme_update(plot.title = element_text(hjust = 0.5))
SB100$ggsealmax <- SB100$GapSealMax - SB100$GapSealMin
SB100$gg2max <- SB100$Gap2Max - SB100$Gap2Min
SB140$ggdae<-SB140$GapDaeMax-SB140$GapDaeMin
SB140p$gg1<-SB140p$Gap1Max-SB140p$Gap1Min
SB140p$gg2<-SB140p$Gap2Max-SB140p$Gap2Min
SB140p$gg3<-SB140p$Gap3Max-SB140p$Gap3Min




ggplot(SB140p,aes(x=gg3,color=as.factor(SCR)))+geom_density()+ggtitle("3�� ����")

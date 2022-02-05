aa<-read.csv("./time222.csv",stringsAsFactors = F)
for (i in tt) {
  aa[,i]<-as.Date(aa[,i])
}

two<-rownames(aa)[!is.na(aa$병리_수술일_01)]
two2<-aa[as.numeric(two),]
one<-aa[-as.numeric(two),]

one2<-as.Date(one[,-c(1,39,40)])
tt<-c(1:40)
tt<-tt[-c(1,39,40)]
num_fol<-NULL
for (i2 in 1:11859){
  num_fol<-append(num_fol,sum(one[i2,-c(1,2,3,39,40)]>=one[i2,2],na.rm = T))
}
one$num_fol <-num_fol
library(ggplot2)
ggplot(one,aes(x= num_fol,y=..count..))+geom_bar()+geom_text(stat='count', aes(label=..count..), vjust=-1)+ggtitle("Number of FollowUp")+ggsave("c:/Users/hussu/Desktop/thy2/fol.png",dpi = 400)
sum(one[1,-c(1,2,3,39,40)]>=one[1,2],na.rm = T)
sum(one$num_fol==1)

?geom_text

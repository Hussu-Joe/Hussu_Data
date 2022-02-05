library(tseries)
library(forecast)
mstime104log<-read.csv("c:/Users/Data1/Desktop/6주차2/mstimelog.csv")
mstime104log$Date<-as.Date(mstime104log$Date)
rownames(mstime104log)<-mstime104log$Date
mstime104log<-mstime104log[,-1]
stat104<-read.csv("c:/Users/Data1/Desktop/6주차2/statff.csv",stringsAsFactors = F)
mstime52log<-read.csv("c:/Users/Data1/Desktop/6주차2/mstime(52).csv")
stat52<-read.csv("c:/Users/Data1/Desktop/6주차2/기초통계_52.csv",stringsAsFactors = F)
stat26<-read.csv("c:/Users/Data1/Desktop/6주차2/기초통계_26.csv",stringsAsFactors = F)


mstime26log<-read.csv("c:/Users/Data1/Desktop/6주차2/mstime(26).csv")

rownames(mstime52log)<-rownames(mstime104log)[seq(1,104,by=2)]
rownames(mstime26log)<-rownames(mstime104log)[seq(1,104,by=4)]
mstime104log<-mstime104log[stat104$item_code[stat104$sample_c>=24]]
mstime52log<-mstime52log[stat104$item_code[stat104$sample_c>=24]]
mstime26log<-mstime26log[stat104$item_code[stat104$sample_c>=24]]
stat52<-subset(stat52,stat52$item_code==colnames(mstime52log))
stat26<-subset(stat26,stat26$item_code==colnames(mstime26log))
inf1<-NULL
for(i in colnames(mstime52log)){
  if(sum(is.infinite(mstime52log[[i]]))>0){
    inf1<-append(inf1,1)
  } else {
    inf1<-append(inf1,0)
  }
}
stat52$inf1<-inf1
mstime52log<-log(mstime52log[stat52$item_code[stat52$inf1==0]])
#### 6weeks203
mstime26log<-log(mstime26log)
inf1<-NULL
for(i in colnames(mstime26log)){
  if(sum(is.infinite(mstime26log[[i]]))>0){
    inf1<-append(inf1,1)
  } else {
    inf1<-append(inf1,0)
  }
}
stat26$inf1<-inf1
mstime26log<-log(mstime26log[stat26$item_code[stat26$inf1==0]])
#### 6weeks204
for(i in colnames(mstime52log)){
  if(stat52$inf1[stat52$item_code==i]==0){
    mstime52log[i]<-log(mstime52log[i])
  }
}

for(i in colnames(mstime26log)){
  if(stat26$inf1[stat26$item_code==i]==0){
    mstime26log[i]<-log(mstime26log[i])
  }
}
#### 6weeks204

a<-as.data.frame(stat104$item_code[is.na(stat104$rmsedivmean)])
colnames(a)<-"item_code"
a$item_code<-as.character(a$item_code)
write.csv(a,"c:/Users/Data1/Desktop/6주차2/rmsedifvmean미싱.csv")

write.csv(a,"c:/Users/Data1/Desktop/6주차2/rmsedifvmean05이하.csv")
write.csv(mstime26log,"c:/Users/Data1/Desktop/6주차2/mstime26log.csv")

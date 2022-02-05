library(readxl)
thy<-read_excel("c:/Users/hussu/Desktop/thy2/20190311_all.xlsx")
time<-read.csv("c:/Users/hussu/Desktop/thy2/time222.csv")
for (i in 2:38) {
  time[,i]<-as.Date(time[,i])
}

time[1,2]

which(time[1,4]<=time[1,2])
max(which(time[50,4:38]>time[50,2]))
time[50,4:38]
pre<-NULL
maxfol<-NULL

for (i in 1:11875) {
  a<-max(which(time[i,4:38]<=time[i,2]))
  pre<-append(pre,a)
  b<-max(which(time[i,4:38]>time[i,2]))
  maxfol<-append(maxfol,b)
}

maxfol[is.infinite(maxfol)]<-NA
pre[is.infinite(pre)]<-NA

thy$maxpre<-pre
thy$maxpost<-maxfol

max(thy$maxpost,na.rm = T)

# 20190410


colnames(thy)<-gsub("내분비","endo",colnames(thy))
thy$`endo_Thyroglobulin Ag[Serum]_00`

ATG<-thy[,which(grepl("Anti-thyroglobulin",colnames(thy)))]
ATPO<-thy[,which(grepl("Anti-TPO",colnames(thy)))]
FT4<-thy[,which(grepl("Free T4",colnames(thy)))]
T3<-thy[,which(grepl("T3",colnames(thy)))]
TSH<-thy[,which(grepl("TSH",colnames(thy)))]
TG<-thy[,which(grepl("Thyroglobulin Ag",colnames(thy)))]
# 2019041101
as.numeric(ATG[1,1:thy$maxpost[1]])
value<-NULL

for (i in 1:11875) {
  a<-rep(NA,34)
  if (is.na(thy$maxpost[i])) {
    value<-append(value,a)
    next
  } else if (is.na(thy$maxpre[i])){
    a[2:length(as.numeric(T3[i,1:(thy$maxpost[i]-1)]))]<-as.numeric(T3[i,1:(thy$maxpost[i]-1)])
    value<-append(value,a)
    next
  } 
  a[1:length(as.numeric(T3[i,thy$maxpre[i]:(thy$maxpost[i]-1)]))]<-as.numeric(T3[i,thy$maxpre[i]:(thy$maxpost[i]-1)])
  value<-append(value,a)
}

times1<-rep(1:34,length.out=403750)

T3<-data.frame(times1,value)
library(ggplot2)
ggplot(T3,aes(x=factor(times1),y=value))+geom_boxplot()+ggtitle("T3")
ggsave("./T3box.png",dpi = 400)
 ?ggplot
### 2019041102

summary(thy$maxpost)
sum(thy$maxpost<5,na.rm = T)
sum(thy$maxpost>=5 & thy$maxpost<=12,na.rm = T)
sum(thy$maxpost>12,na.rm = T)
sum(is.na(thy$maxpost))

2152+6512+2802+409

FU_cat<-rep(NA,11875)

for (i in 1:11875) {
  if (is.na(thy$maxpost[i])) {
    FU_cat[i]<-4
    next
  } else if(thy$maxpost[i]>12){
    FU_cat[i]<-3
    next
  } else if(thy$maxpost[i]<=12 & thy$maxpost[i]>=5){
    FU_cat[i]<-2
    next
  } else if(thy$maxpost[i]<5){
    FU_cat[i]<-1
    next
  }
}

thy$fu_cat<-as.factor(FU_cat)

# 2019041501
ATG_1<-ATG[which(thy$fu_cat==1),]
ATG_2<-ATG[which(thy$fu_cat==2),]
ATG_3<-ATG[which(thy$fu_cat==3),]



colnames(thy)[891]
write.csv(thy,"./thy.csv")


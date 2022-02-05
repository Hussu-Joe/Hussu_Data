mstime<-read.csv("c:/Users/hussu/Desktop/4주차/mstime2.csv",stringsAsFactors = F)
mstime<-mstime[,-1]
weather<-read.csv("c:/Users/hussu/Desktop/4주차/weatherfinal.csv")
mstime2<-NULL
for (i in 1:52) {
  t<-mstime[(2*i-1),]+mstime[(2*i),]
  mstime2<-rbind(mstime2,t)
}
a<-mstime[1,]+mstime[2,]

write.table(mstime2,"c:/Users/hussu/Desktop/4주차/mstime(52).csv",sep = ",")

mstime3<-NULL
for (i in 1:26){
  t<-mstime[(4*i-3),]+mstime[(4*i-2),]+mstime[(4*i-1),]+mstime[(4*i),]
  mstime3<-rbind(mstime3,t)
}
write.table(mstime3,"c:/Users/hussu/Desktop/4주차/mstime(26).csv",sep = ",")

#060202
weather2<-NULL
for (i in 1:52) {
  t<-(weather[(2*i-1),]+weather[(2*i),])/2
  weather2<-rbind(weather2,t)
}

weather3<-NULL
for (i in 1:26){
  t<-(weather[(4*i-3),]+weather[(4*i-2),]+weather[(4*i-1),]+weather[(4*i),])/4
  weather3<-rbind(weather3,t)
}

write.table(weather2,"c:/Users/hussu/Desktop/4주차/weather(52).csv",sep = ",")
write.table(weather3,"c:/Users/hussu/Desktop/4주차/weather(26).csv",sep = ",")
#060203
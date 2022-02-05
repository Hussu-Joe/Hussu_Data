library(readxl)
thy<-read_excel("c:/Users/hussu/Desktop/thy2/20190311_all.xlsx")
colnames(thy)
thy$병리_LYMPH_C_IRRA_00<-as.numeric(thy$병리_LYMPH_C_IRRA_00)

irra_00<-thy$등록번호[!is.integer(thy$병리_LYMPH_C_IRRA_00)]
sum(is.integer(thy$병리_LYMPH_C_IRRA_00))

tail(summary(as.factor(thy$`내분비_Anti-TPO Ab[Serum]_06`),maxsum=1000000),10)
thy[,"내분비_Anti-TPO Ab[Serum]_06"]

tail(summary(as.factor(thy$`내분비_Anti-thyroglobulin Ab[Serum]_00`),maxsum=1000000),100)


tail(summary(as.factor(thy$`내분비_Free T4[Serum]_00`),maxsum=1000000),100)
tail(summary(as.factor(thy$`내분비_T3[Serum]_00`),maxsum=1000000),100)
tail(summary(as.factor(thy$`내분비_TSH[Serum]_00`),maxsum=1000000),100)
tail(summary(as.factor(thy$`내분비_Thyroglobulin Ag[Serum]_00`),maxsum=1000000),100)
boxplot(thy$`내분비_Anti-TPO Ab[Serum]_00`)
time<-thy[,c(1,2,23,seq(from=58,by=8,length.out = 35))]


duration2<-NULL
for (i in 1:11875) {
  ttt<-NULL
  kkk<-NULL
  for(i2 in 4:38){
    a<-time[i,2]-time[i,i2]
    if(is.na(a)){
      next
    } else if(a>0){
      ttt<-append(ttt,as.numeric(a)) 
    }
  }
  ttt<-ttt[!is.na(ttt)]
  duration2<-append(duration2,min(ttt))
}

time$dura2<-duration2
write.csv(time22,"c:/Users/hussu/Desktop/thy2/time222.csv",sep = ",")
time22<-time
time22$등록번호[time22$dura<=-90]

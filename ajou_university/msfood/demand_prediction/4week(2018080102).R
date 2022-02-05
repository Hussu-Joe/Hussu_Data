
length(mstime$X118117[!is.na(mstime$X118117)])


11*3448
sum_ms<-data.frame(matrix(rep(NA,37928),ncol = 11))
colnames(sum_ms)<-c("item_code","mean","std","acf_time","acf_value","temp2_time","temp2_value","prec_time","prec_value","sample_c","zeroc")
sum_ms$item_code<-colnames(mstime)
for (i in 1:3448) {
  sum_ms$mean[i]<-round(mean(mstime[,sum_ms$item_code[i]],na.rm = T),3)
}

for (i in 1:3448) {
  sum_ms$std[i]<-round(sqrt(var(mstime[,sum_ms$item_code[i]],na.rm = T)),3)
}

for(i in 1:3448){
  sum_ms$sample_c[i]<-104-sum(is.na(mstime[,sum_ms$item_code[i]]))
}
for(i in 1:3448){
  sum_ms$zeroc[i]<-round((length(which(mstime[,sum_ms$item_code[i]]==0)))/(sum_ms$sample_c[i]),3)
}




for(i in 2549:3448){
  if(sum_ms$mean[i]==0){
    next
  }
  t<-acf(mstime[,sum_ms$item_code[i]],na.action = na.exclude,lag.max = 8)
  if(abs(max(t$acf))>=abs(min(t$acf))){
    sum_ms$acf_time[i]<-which(t$acf==max(t$acf[2:9]))
    sum_ms$acf_value[i]<-round(t$acf[sum_ms$acf_time[i]],3)
  } else {
    sum_ms$acf_time[i]<-which(t$acf==min(t$acf[2:9]))
    sum_ms$acf_value[i]<-round(t$acf[sum_ms$acf_time[i]],3)
  }
}
# 080108



sum_ms$acf_time[!is.na(sum_ms$acf_time)]<-sum_ms$acf_time[!is.na(sum_ms$acf_time)]-1
sum_ms$prec_time[!is.na(sum_ms$prec_time)]<-sum_ms$prec_time[!is.na(sum_ms$prec_time)]-5
sum_ms$temp2_time[!is.na(sum_ms$temp2_time)]<-sum_ms$temp2_time[!is.na(sum_ms$temp2_time)]-5


p<-ccf(weather$temp2,mstime$X106325,na.action = na.exclude,lag.max = 4)
p
summary(p$acf)

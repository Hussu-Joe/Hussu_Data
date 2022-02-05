library(tseries)
library(forecast)

mstimelog104<-read.csv("c:/Users/hussu/Desktop/7주차/mstime104log.csv")
rownames(mstimelog104)<-as.Date(mstimelog104$X)
mstimelog104<-mstimelog104[,-1]
stat104<-read.csv("c:/Users/hussu/Desktop/7주차/stat104.csv",stringsAsFactors = F)
rmsedivmean<-read.csv("c:/Users/hussu/Desktop/7주차/stat104_1.csv",stringsAsFactors = F)
rmsedivmean<-rmsedivmean$item_code
fit1<-as.data.frame(matrix(rep(NA,95472),ncol = 918))
predict1<-as.data.frame(matrix(rep(NA,3672),ncol=918))
colnames(predict1)<-rmsedivmean$item_code
colnames(fit1)<-rmsedivmean$item_code
unitr<-NULL
for (i in colnames(mstimelog104)) {
  tryCatch({
    p<-adf.test(mstimelog104[[i]][!is.na(mstimelog104[[i]])])
  }, error = function(e){cat("ERROR :", conditionMessage(e),"\n")})
  unitr<-append(unitr,p$p.value)
}





unit2<-NULL

for(i in colnames(mstimelog104)){
  if (is.na(i)) {
    unit2<-append(unit2,NA)
    next
  }
  tryCatch({
    p<-adf.test(diff(mstimelog104[[i]][!is.na(mstimelog104[[i]])]))
  }, error = function(e){cat("ERROR :", conditionMessage(e),"\n")})
  unit2<-append(unit2,p$p.value)
}

stat104$unit1<-unitr
stat104$unit2<-unit2

stat104$dif<-rep(NA,2255)
stat104$dif[stat104$unit1<0.05]<-0
stat104$dif[is.na(stat104$unit1)]<-0
stat104$dif[stat104$unit1>0.05 & stat104$unit2<0.05]<-1
stat104$dif[stat104$unit1>0.05 & stat104$unit2>0.05]<-2

lappend<-function(lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}



list1<-list()
aaa<-c(0,1,2)
aaa2<-c(0,1,2,3)
for(i in aaa2){
  for(i2 in aaa){
    for(i3 in aaa){
      for(i4 in aaa){
        for(i5 in aaa){
          list1<-lappend(list1,c(i,i2,i3,i4,i5))
        }
      }
    }
  }
}

#7weeks2

setwd("c:/Users/hussu/Desktop/7주차/104/")
for (t1 in rmsedivmean[1]) {
  rmse=NULL
  for(t2 in list1){
    tryCatch({
      model = arima(mstimelog104[t1],order = c(t2[1],stat104$dif[stat104$item_code==t1],t2[2]), seasonal = list(order=c(t2[3],t2[4],t2[5]),period=16))
    }, error =function(e){cat("ERROR :", conditionMessage(e), "\n")})
    rmse = append(rmse,accuracy(model)[2])
  }
  min_index = which.min(rmse)
  min_order = as.vector(list1[min_index])
  model2<-arima(mstimelog104[t1],order = c(min_order[[1]][1],stat104$dif[stat104$item_code==t1],min_order[[1]][2]),seasonal = list(order=c(min_order[[1]][3],min_order[[1]][4],min_order[[1]][5]),period=16))
  fittv<-as.vector(fitted.values(model2))
  fit1[[t1]]<-fittv
  predict1[[t1]]<-as.vector(predict(model2,n.ahead = 4)$pred)
  if(stat104$inf1[stat104$item_code==t1]==0){
    png(paste0(t1,".png"))
    plot(append(exp(fit1[[t1]]),exp(predict1[[t1]])),type = "l",col="red")
    lines(exp(mstimelog104[[t1]]),col="blue")
    title(t1)
    dev.off()
  } else {
    png(paste0(t1,".png"))
    plot(append(fit1[[t1]],predict1[[t1]]),type = "l",col="red")
    lines(mstimelog104[[t1]],col="blue")
    title(t1)
    dev.off()
  }
}

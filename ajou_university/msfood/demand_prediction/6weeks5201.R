library(tseries)
library(forecast)
mstimelog52<-read.csv("c:/Users/Data1/Desktop/6주차2/mstime52log.csv")
stat52<-read.csv("c:/Users/Data1/Desktop/6주차2/stat52.csv",stringsAsFactors = F)
rmsedivmean<-read.csv("c:/Users/Data1/Desktop/6주차2/rmsedifvmean05이상1미만.csv",stringsAsFactors = F)
lappend<-function(lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}


list1<-list()
aaa<-c(0,1,2)
for(i in aaa){
  for(i2 in aaa){
    for(i3 in aaa){
      for(i4 in aaa){
        for(i5 in aaa){
          for(i6 in aaa){
            list1<-lappend(list1,c(i,i2,i3,i4,i5,i6))
          }
        }
      }
    }
  }
}

fit1<-as.data.frame(matrix(rep(NA,34476),ncol = 663))
predict1<-as.data.frame(matrix(rep(NA,1326),ncol=663))
colnames(predict1)<-rmsedivmean$item_code
colnames(fit1)<-rmsedivmean$item_code

setwd("c:/Users/Data1/Desktop/6주차2/52/")

for (t1 in rmsedivmean$item_code[11:30]) {
  rmse=NULL
  for(t2 in list1){
    tryCatch({
      model = arima(mstimelog52[t1],order = c(t2[1],t2[2],t2[3]), seasonal = list(order=c(t2[4],t2[5],t2[6]),period=8))
    }, error =function(e){cat("ERROR :", conditionMessage(e), "\n")})
    rmse = append(rmse,accuracy(model)[2])
  }
  min_index = which.min(rmse)
  min_order = as.vector(list1[min_index])
  model2<-arima(mstimelog52[t1],order = c(min_order[[1]][1],min_order[[1]][2],min_order[[1]][3]),seasonal = list(order=c(min_order[[1]][4],min_order[[1]][5],min_order[[1]][6]),period=8))
  fittv<-as.vector(fitted.values(model2))
  fit1[[t1]]<-fittv
  predict1[[t1]]<-as.vector(predict(model2,n.ahead = 2)$pred)
  if(stat52$inf1[stat52$item_code==t1]==0){
    png(paste0(t1,".png"))
    plot(append(exp(fit1[[t1]]),exp(predict1[[t1]])),type = "l",col="red")
    lines(exp(mstimelog52[[t1]]),col="blue")
    title(t1)
    dev.off()
  } else {
    png(paste0(t1,".png"))
    plot(append(fit1[[t1]],predict1[[t1]]),type = "l",col="red")
    lines(mstimelog52[[t1]],col="blue")
    title(t1)
    dev.off()
  }
}

#02
unitr<-NULL
for (i in colnames(mstimelog52)) {
  tryCatch({
    p<-adf.test(mstimelog52[[i]][!is.na(mstimelog52[[i]])])
  }, error = function(e){cat("ERROR :", conditionMessage(e),"\n")})
  unitr<-append(unitr,p$p.value)
}
unit2<-NULL

for(i in colnames(mstimelog52)){
  if (is.na(i)) {
    unit2<-append(unit2,NA)
    next
  }
  tryCatch({
    p<-adf.test(diff(mstimelog52[[i]][!is.na(mstimelog52[[i]])]))
  }, error = function(e){cat("ERROR :", conditionMessage(e),"\n")})
  unit2<-append(unit2,p$p.value)
}

stat52$unit1<-unitr
stat52$unit2<-unit2


list1<-list()
aaa<-c(0,1,2)
aaa2<-c(0,1,2,3)
for(i in aaa){
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

stat52$dif<-rep(NA,2255)
stat52$dif[stat52$unit1<0.05]<-0
stat52$dif[is.na(stat52$unit1)]<-0
stat52$dif[stat52$unit1>0.05 & stat52$unit2<0.05]<-1
stat52$dif[stat52$unit1>0.05 & stat52$unit2>0.05]<-2
stat52$dif[is.na(stat52$dif)]<-1




setwd("c:/Users/Data1/Desktop/6주차2/52/")
for (t1 in rmsedivmean$item_code) {
  rmse=NULL
  for(t2 in list1){
    tryCatch({
      model = arima(mstimelog52[t1],order = c(t2[1],stat52$dif[stat52$item_code==t1],t2[2]), seasonal = list(order=c(t2[3],t2[4],t2[5]),period=6))
    }, error =function(e){cat("ERROR :", conditionMessage(e), "\n")})
    rmse = append(rmse,accuracy(model)[2])
  }
  min_index = which.min(rmse)
  min_order = as.vector(list1[min_index])
  model2<-arima(mstimelog52[t1],order = c(min_order[[1]][1],stat52$dif[stat52$item_code==t1],min_order[[1]][2]),seasonal = list(order=c(min_order[[1]][3],min_order[[1]][4],min_order[[1]][5]),period=6))
  fittv<-as.vector(fitted.values(model2))
  fit1[[t1]]<-fittv
  predict1[[t1]]<-as.vector(predict(model2,n.ahead = 2)$pred)
  if(stat52$inf1[stat52$item_code==t1]==0){
    png(paste0(t1,".png"))
    plot(append(exp(fit1[[t1]]),exp(predict1[[t1]])),type = "l",col="red")
    lines(exp(mstimelog52[[t1]]),col="blue")
    title(t1)
    dev.off()
  } else {
    png(paste0(t1,".png"))
    plot(append(fit1[[t1]],predict1[[t1]]),type = "l",col="red")
    lines(mstimelog52[[t1]],col="blue")
    title(t1)
    dev.off()
  }
}
#03

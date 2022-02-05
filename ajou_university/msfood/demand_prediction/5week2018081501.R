mstime104<-read.csv("c:/Users/hussu/Desktop/5주차/mstime2.csv", row.names = 1)
mstime52<-read.csv("c:/Users/hussu/Desktop/5주차/mstime(52).csv")
mstime26<-read.csv("c:/Users/hussu/Desktop/5주차/mstime(26).csv",row.names = 1)
weather104<-read.csv("c:/Users/hussu/Desktop/5주차/weatherfinal.csv")
weather52<-read.csv("c:/Users/hussu/Desktop/5주차/weather(52).csv",row.names = 1)
weather26<-read.csv("c:/Users/hussu/Desktop/5주차/weather(26).csv",row.names = 1)
stat104<-read.csv("c:/Users/hussu/Desktop/5주차/기초통계.csv")
stat52<-read.csv("c:/Users/hussu/Desktop/5주차/기초통계_52.csv")
stat26<-read.csv("c:/Users/hussu/Desktop/5주차/기초통계_26.csv")

library(tseries)
library(forecast)
c<-auto.arima(log(mstime104$X),D=1)
c

auto.arima()


a<-adf.test(diff(log(mstime104$X106320)),k = 0)
a$p.value
?auto.arima

test1<-arima(log(mstime104$X106320[1:100]),order = c(2,2,1),seasonal = list(order= c(0,0,0),period=4))

test1$



test2<-predict(test1,n.ahead = 4)

exp(test2$pred)
mstime104$X106320[101:104]

exp(log(mstime104$X106320[101]))

?adf.test

test1<-function(x){
  require(tseries)
  require(forecast)
  dif<-0
  t<-x[1:(length(x)-4)]
  unit_root1<-adf.test(t,k=0)
  if(unit_root1$p.value>0.05){
    dif<-dif+1
    t1<-diff(log(t))
    unit_root2<-adf.test(t1,k=0)
    if(unit_root2$p.value>0.05){
      dif<-dif+1
    }
  }
  t<-log(t)
  autoa<-auto.arima(t)
  model1<-arima(t,order = c(autoa$arma[1],dif,autoa$arma[2]),seasonal = list(order= c(3,1,1),period= 4))
  pred1<-predict(model1,n.ahead = 4)
  predict4<-exp(pred1$pred)
  real4<-x[(length(x)-3):length(x)]
  ret1<-data.frame(real4,predict4)
  return(ret1)
}

test1(mstime104$X106320)

aaa<-c(0,1,2)
aaa2<-c(0,1)
list1<-list()


lappend <- function(lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}



for (t in aaa2) {
  p<-NULL
  for (t1 in aaa) {
    for (t2 in aaa2) {
      p<-c(t,t1,t2)
      list1<-lappend(list1,p)
    }
  }
}

test2<-function(x){
  require(tseries)
  require(forecast)
  t<-x[1:(length(x)-4)]
  rmse<-NULL
  for (p in list1) {
    for (pp in list1) {
      model1<-arima(log(t),order = c(p),seasonal = list(order=c(pp),period=4))
      pred1<-predict(model1,n.ahead=4)
      real1<-x[(length(x)-3):length(x)]
      ret1<-data.frame(real1,exp(pred1$pred))
      ret1$diff = sqrt((ret1[,1]-ret1[,2])^2)
      print(cat(p,pp))
      print(sum(ret1$diff)/4)
    }
  }
}
mstime104$X106320

test2(mstime104$X106320)

ppppppppp<-accuracy(test1)[2]






dif<-0
t<-mstime104$X106320[1:100]
unit_root1<-adf.test(t,k=0)
unit_root1$p.value

if(unit_root1$p.value<0.05){
  next
} else if(unit_root1$p.value>0.05){
  t<-diff(log(t))
  dif<-dif+1
  unit_root2<-adf.test(t,k=0)
}
if(unit_root2$p.value)

print(3)

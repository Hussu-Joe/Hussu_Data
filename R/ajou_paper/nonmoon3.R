library(urca)
library(ggplot2)
library(tseries)
library(forecast)
library(ZIM)
library(dyn)
library(Metrics)
library(lmtest)
# X112530 1 1 1 ITEM1
# X105868 2 1 2 ITEM2
# X108939 0 1 1 ITEM3
# X112506 0 1 1 ITEM4
# X114143 1 1 1 ITEM5
# X105837 2 1 2 ITEM6
daily<-read.csv("c:/Users/hussu/Desktop/nonmoon/daily20190512.csv",row.names = 1)

p<-c(0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2)
d<-c(0,0,0,1,1,1,2,2,2,0,0,0,1,1,1,2,2,2,0,0,0,1,1,1,2,2,2)
q<-c(0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2)

ord<-data.frame(p,d,q)

rmsemae<-function(ord,t,met){
  require(Metrics)
  require(forecast)
  arimord<-NULL
  arrmse<-NULL
  armae<-NULL
  armet<-rep(met,27)
  
  for (i in 1:27) {
    arimord1<-paste0("ord",ord$p[i],ord$d[i],ord$q[i])
    arimord<-append(arimord,arimord1)
    tryCatch({
      model1<-arima(t[1:437],order = c(ord$p[i],ord$d[i],ord$q[i]),method = met)
      rmse1<-rmse(t[438:624],as.numeric(predict(model1,187)[[1]]))
      mae1<-mae(t[438:624],as.numeric(predict(model1,187)[[1]]))
      arrmse<-append(arrmse,rmse1)
      armae<-append(armae,mae1)
    },error=function(e){cat("ERROR :", conditionMessage(e),"\n")})
  }
  newdf1<-data.frame(arimord,arrmse,armae,armet)
  return(newdf1)
}

item1_css<-rmsemae(ord,daily$ITEM1,"CSS")
item2_css<-rmsemae(ord,daily$ITEM2,"CSS")
item3_css<-rmsemae(ord,daily$ITEM3,"CSS")
item4_css<-rmsemae(ord,daily$ITEM4,"CSS")
item5_css<-rmsemae(ord,daily$ITEM5,"CSS")
item6_css<-rmsemae(ord,daily$ITEM6,"CSS")


item1_ml<-rmsemae(ord,daily$ITEM1,"ML")
item2_ml<-rmsemae(ord,daily$ITEM2,"ML")
item3_ml<-rmsemae(ord,daily$ITEM3,"ML")
item5_ml<-rmsemae(ord,daily$ITEM5,"ML")
item6_ml<-rmsemae(ord,daily$ITEM6,"ML")


item1_cssml<-rmsemae(ord,daily$ITEM1,"CSS-ML")
item2_cssml<-rmsemae(ord,daily$ITEM2,"CSS-ML")
item3_cssml<-rmsemae(ord,daily$ITEM3,"CSS-ML")
item4_cssml<-rmsemae(ord,daily$ITEM4,"CSS-ML")
item5_cssml<-rmsemae(ord,daily$ITEM5,"CSS-ML")
item6_cssml<-rmsemae(ord,daily$ITEM6,"CSS-ML")






# item4 ML
item4_100_ML<-arima(daily$ITEM4[1:437],c(1,0,0),method = "ML")     #49.05051  37.19807
item4_101_ML<-arima(daily$ITEM4[1:437],c(1,0,1),method = "ML")     #49.05671  37.2073
item4_102_ML<-arima(daily$ITEM4[1:437],c(1,0,2),method = "ML")     #47.79773  36.53399
item4_110_ML<-arima(daily$ITEM4[1:437],c(1,1,0),method = "ML")    # 52.61574   43.12609
#item4_111_ML<-arima(daily$ITEM4[1:437],c(1,1,1),method = "ML")    #49.02779   36.71918
item4_112_ML<-arima(daily$ITEM4[1:437],c(1,1,2),method = "ML")    #47.88981   36.48962
item4_120_ML<-arima(daily$ITEM4[1:437],c(1,2,0),method = "ML")    #2126.083   1853.222
item4_121_ML<-arima(daily$ITEM4[1:437],c(1,2,1),method = "ML")    #53.41499   44.32421
item4_122_ML<-arima(daily$ITEM4[1:437],c(1,2,2),method = "ML")    #49.30798   36.3107

item4_200_ML<-arima(daily$ITEM4[1:437],c(2,0,0),method = "ML")    # 49.07155    37.22117
item4_201_ML<-arima(daily$ITEM4[1:437],c(2,0,1),method = "ML")     #47.70264  36.60316
item4_202_ML<-arima(daily$ITEM4[1:437],c(2,0,2),method = "ML")     #47.82766  36.52184
item4_210_ML<-arima(daily$ITEM4[1:437],c(2,1,0),method = "ML")    # 51.90776   42.18135
item4_211_ML<-arima(daily$ITEM4[1:437],c(2,1,1),method = "ML")    #  49.03229  36.64308
item4_212_ML<-arima(daily$ITEM4[1:437],c(2,1,2),method = "ML")     #47.65649  36.14415
item4_220_ML<-arima(daily$ITEM4[1:437],c(2,2,0),method = "ML")    #1552.473  1355.087
item4_221_ML<-arima(daily$ITEM4[1:437],c(2,2,1),method = "ML")    # 52.45311   43.03075
item4_222_ML<-arima(daily$ITEM4[1:437],c(2,2,2),method = "ML")    # 50.20209   35.88578

item4_001_ML<-arima(daily$ITEM4[1:437],c(0,0,1),method = "ML")      #49.05668  37.19808
item4_002_ML<-arima(daily$ITEM4[1:437],c(0,0,2),method = "ML")      #49.06832 37.22757
item4_010_ML<-arima(daily$ITEM4[1:437],c(0,1,0),method = "ML")     #58.95403  51.08556
item4_011_ML<-arima(daily$ITEM4[1:437],c(0,1,1),method = "ML")     # 49.04148  36.69094
item4_012_ML<-arima(daily$ITEM4[1:437],c(0,1,2),method = "ML")     # 49.02809  36.65587
item4_020_ML<-arima(daily$ITEM4[1:437],c(0,2,0),method = "ML")     #103.4405  92.4425
item4_021_ML<-arima(daily$ITEM4[1:437],c(0,2,1),method = "ML")    # 121.1818  105.4905
item4_022_ML<-arima(daily$ITEM4[1:437],c(0,2,2),method = "ML")    # 121.1818  105.4905




rmse(daily$ITEM4[438:624],as.numeric(predict(item4_022_ML,187)[[1]]))
mae(daily$ITEM4[438:624],as.numeric(predict(item4_022_ML,187)[[1]]))


rmsemae90<-function(ord,t,met){
  require(Metrics)
  require(forecast)
  arimord<-NULL
  arrmse<-NULL
  armae<-NULL
  armet<-rep(met,27)
  
  for (i in 1:27) {
    arimord1<-paste0("ord",ord$p[i],ord$d[i],ord$q[i])
    arimord<-append(arimord,arimord1)
    tryCatch({
      model1<-arima(t[1:437],order = c(ord$p[i],ord$d[i],ord$q[i]),method = met)
      rmse1<-rmse(t[438:527],as.numeric(predict(model1,90)[[1]]))
      mae1<-mae(t[438:527],as.numeric(predict(model1,90)[[1]]))
      arrmse<-append(arrmse,rmse1)
      armae<-append(armae,mae1)
    },error=function(e){cat("ERROR :", conditionMessage(e),"\n")})
  }
  newdf1<-data.frame(arimord,arrmse,armae,armet)
  return(newdf1)
}

test<-lm(dyn(daily2$ITEM1~lag(daily2$ITEM1,-1)))

ar1fore<-function(x){
  mod1<-lm(dyn(x[1:437]~lag(x[1:437],-1)))
  coin<-mod1$coefficients[[1]]
  ar1<-mod1$coefficients[[2]]
  fore1<-as.numeric(x[437])
  for (i in 1:187) {
    aaa1<-coin+fore1[i]*ar1
    fore1<-append(fore1,aaa1)
  }
  return(fore1)
}

ar2fore<-function(x){
  mod1<-lm(dyn(x[1:437]~lag(x[1:437],-1)+lag(x[1:437],-2)))
  coin<-mod1$coefficients[[1]]
  ar1<-mod1$coefficients[[2]]
  ar2<-mod1$coefficients[[3]]
  fore1<-as.numeric(c(x[436],x[437]))
  for (i in 1:187) {
    aaa1<-coin+fore1[i+1]*ar1+fore1[i]*ar2
    fore1<-append(fore1,aaa1)
  }
  return(fore1)
}


ar3fore<-function(x){
  mod1<-lm(dyn(x[1:437]~lag(x[1:437],-1)+lag(x[1:437],-2)+lag(x[1:437],-3)))
  coin<-mod1$coefficients[[1]]
  ar1<-mod1$coefficients[[2]]
  ar2<-mod1$coefficients[[3]]
  ar3<-mod1$coefficients[[4]]
  fore1<-as.numeric(c(x[435],x[436],x[437]))
  for (i in 1:187) {
    aaa1<-coin+fore1[i+2]*ar1+fore1[i+1]*ar2+fore1[i]*ar3
    fore1<-append(fore1,aaa1)
  }
  return(fore1)
}



ar6fore<-function(x){
  mod1<-lm(dyn(x[1:437]~lag(x[1:437],-1)+lag(x[1:437],-2)+lag(x[1:437],-3)+
                 lag(x[1:437],-4)+lag(x[1:437],-5)+lag(x[1:437],-6)))
  coin<-mod1$coefficients[[1]]
  ar1<-mod1$coefficients[[2]]
  ar2<-mod1$coefficients[[3]]
  ar3<-mod1$coefficients[[4]]
  ar4<-mod1$coefficients[[5]]
  ar5<-mod1$coefficients[[6]]
  ar6<-mod1$coefficients[[7]]
  fore1<-as.numeric(c(x[432],x[433],x[434],x[435],x[436],x[437]))
  for (i in 1:187) {
    aaa1<-coin+fore1[i+5]*ar1+fore1[i+4]*ar2+fore1[i+3]*ar3+
      ar4*fore1[i+2]+ar5*fore1[i+1]+ar6*fore1[i]
    fore1<-append(fore1,aaa1)
  }
  return(fore1)
}




daily2$ITEM1_ma1

predict(arima(daily$ITEM4[1:619],c(2,0,0)),5)$pred
rmse(predict(ar.ols(daily$ITEM3[1:618],order.max =6),6)$pred,daily$ITEM3[619:624])


daily$ITEM4[620:624]


AIC(arima(daily$ITEM1[1:437],c(2,0,1)))
lm(dyn(daily2$ITEM1[1:437]~lag(daily2$ITEM1[1:437],-1)+lag(daily2$ITEM1[1:437],-2)+lag(daily2$ITEM1[1:437],-3)))

ar6fore(daily2$ITEM6)[7:193]
daily$ITEM6[438:624]

rmse(ar6fore(daily2$ITEM1)[7:193],daily$ITEM1[438:624])
mae(ar6fore(daily2$ITEM1)[7:193],daily$ITEM1[438:624])



ar.ols(daily2$ITEM1,order.max = 2)
arima
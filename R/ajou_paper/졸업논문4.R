library(urca)
library(ggplot2)
#library(tseries)
library(forecast)
library(ZIM)
library(dyn)
library(Metrics)
library(lmtest)
library(nnet)

daily<-read.csv("c:/Users/hussu/Desktop/nonmoon/daily20190512.csv",row.names = 1)
daily<-daily[,-c(7:12)]
daily_zoo<-daily
for (i in 1:6) {
  daily_zoo[,i]<-zoo(daily_zoo[,i])
}

item1_ar6<-lm(dyn(daily_zoo$ITEM1[1:437]~lag(daily_zoo$ITEM1[1:437],-1)+lag(daily_zoo$ITEM1[1:437],-2)+
                    lag(daily_zoo$ITEM1[1:437],-3)+lag(daily_zoo$ITEM1[1:437],-4)+
                    lag(daily_zoo$ITEM1[1:437],-5)+lag(daily_zoo$ITEM1[1:437],-6)))
item2_ar6<-lm(dyn(daily_zoo$ITEM2[1:437]~lag(daily_zoo$ITEM2[1:437],-1)+lag(daily_zoo$ITEM2[1:437],-2)+
                    lag(daily_zoo$ITEM2[1:437],-3)+lag(daily_zoo$ITEM2[1:437],-4)+
                    lag(daily_zoo$ITEM2[1:437],-5)+lag(daily_zoo$ITEM2[1:437],-6)))
item3_ar6<-lm(dyn(daily_zoo$ITEM3[1:437]~lag(daily_zoo$ITEM3[1:437],-1)+lag(daily_zoo$ITEM3[1:437],-2)+
                    lag(daily_zoo$ITEM3[1:437],-3)+lag(daily_zoo$ITEM3[1:437],-4)+
                    lag(daily_zoo$ITEM3[1:437],-5)+lag(daily_zoo$ITEM3[1:437],-6)))
item4_ar6<-lm(dyn(daily_zoo$ITEM4[1:437]~lag(daily_zoo$ITEM4[1:437],-1)+lag(daily_zoo$ITEM4[1:437],-2)+
                    lag(daily_zoo$ITEM4[1:437],-3)+lag(daily_zoo$ITEM4[1:437],-4)+
                    lag(daily_zoo$ITEM4[1:437],-5)+lag(daily_zoo$ITEM4[1:437],-6)))

item5_ar6<-lm(dyn(daily_zoo$ITEM5[1:437]~lag(daily_zoo$ITEM5[1:437],-1)+lag(daily_zoo$ITEM5[1:437],-2)+
                    lag(daily_zoo$ITEM5[1:437],-3)+lag(daily_zoo$ITEM5[1:437],-4)+
                    lag(daily_zoo$ITEM5[1:437],-5)+lag(daily_zoo$ITEM5[1:437],-6)))

item6_ar6<-lm(dyn(daily_zoo$ITEM6[1:437]~lag(daily_zoo$ITEM6[1:437],-1)+lag(daily_zoo$ITEM6[1:437],-2)+
                    lag(daily_zoo$ITEM6[1:437],-3)+lag(daily_zoo$ITEM6[1:437],-4)+
                    lag(daily_zoo$ITEM6[1:437],-5)+lag(daily_zoo$ITEM6[1:437],-6)))

dwtest(item6_ar6)

length(lag(daily$ITEM1,-1))
lag(daily$ITEM1,-1)
madf<-function(x){
  t<-length(x)
  ar1<-c(NA,x[1:(t-1)])
  ar2<-c(NA,NA,x[1:(t-2)])
  ar3<-c(NA,NA,NA,x[1:(t-3)])
  ar4<-c(NA,NA,NA,NA,x[1:(t-4)])
  ar5<-c(NA,NA,NA,NA,NA,x[1:(t-5)])
  ar6<-c(NA,NA,NA,NA,NA,NA,x[1:(t-6)])
  newdf<-data.frame(ar1,ar2,ar3,ar4,ar5,ar6)
  return(newdf[-c(1:6),])
}

item1<-madf(daily$ITEM1)
item2<-madf(daily$ITEM2)
item3<-madf(daily$ITEM3)
item4<-madf(daily$ITEM4)
item5<-madf(daily$ITEM5)
item6<-madf(daily$ITEM6)
618-431
item1_p_tr<-item1[1:431,]
item1_o_tr<-matrix(daily$ITEM1[7:437],ncol = 1)
item1_p_te<-item1[432:618,]
item1_o_te<-daily$ITEM1[438:624]



nnetfit_item1<-nnet(item1_p_tr, item1_o_tr,
              size= 5,
              decay = 0.01,
              linout = TRUE,
              trace = FALSE,
              maxit = 500,
              MaxNWts = 5*(ncol(item1_p_tr)+1)+5+1
                )

item1_ann<-as.data.frame(matrix(item1_o_tr[431:426],ncol = 6))
item1_ann_pred<-NULL
for (i in 1:187) {
  a<-predict(nnetfit_item1,item1_ann[i,])
  item1_ann_pred<-append(item1_ann_pred,a)
  item1_ann[i+1,]<-c(a,as.numeric(item1_ann[i,1:5]))
}

length(item1_ann_pred)
rmse(daily$ITEM1[438:624],item1_ann_pred)
mae(daily$ITEM1[438:624],item1_ann_pred)



###item2 ar6

item2_p_tr<-item2[1:431,]
item2_o_tr<-matrix(daily$ITEM2[7:437],ncol = 1)
item2_p_te<-item2[432:618,]
item2_o_te<-daily$ITEM2[438:624]



nnetfit_item2<-nnet(item2_p_tr, item2_o_tr,
                    size= 5,
                    decay = 0.01,
                    linout = TRUE,
                    trace = FALSE,
                    maxit = 500,
                    MaxNWts = 5*(ncol(item2_p_tr)+1)+5+1
)

item2_ann<-as.data.frame(matrix(item2_o_tr[431:426],ncol = 6))
item2_ann_pred<-NULL
for (i in 1:187) {
  a<-predict(nnetfit_item2,item2_ann[i,])
  item2_ann_pred<-append(item2_ann_pred,a)
  item2_ann[i+1,]<-c(a,as.numeric(item2_ann[i,1:5]))
}

rmse(daily$ITEM2[438:624],item2_ann_pred)
mae(daily$ITEM2[438:624],item2_ann_pred)


### item3 ar6

item3_p_tr<-item3[1:431,]
item3_o_tr<-matrix(daily$ITEM3[7:437],ncol = 1)
item3_p_te<-item3[432:618,]
item3_o_te<-daily$ITEM3[438:624]



nnetfit_item3<-nnet(item3_p_tr, item3_o_tr,
                    size= 5,
                    decay = 0.01,
                    linout = TRUE,
                    trace = FALSE,
                    maxit = 500,
                    MaxNWts = 5*(ncol(item3_p_tr)+1)+5+1
)
item3_ann<-as.data.frame(matrix(item3_o_tr[431:426],ncol = 6))
item3_ann_pred<-NULL
for (i in 1:187) {
  a<-predict(nnetfit_item3,item3_ann[i,])
  item3_ann_pred<-append(item3_ann_pred,a)
  item3_ann[i+1,]<-c(a,as.numeric(item3_ann[i,1:5]))
}

rmse(daily$ITEM3[438:624],item3_ann_pred)
mae(daily$ITEM3[438:624],item3_ann_pred)


### item4 ar6

item4_p_tr<-item4[1:431,]
item4_o_tr<-matrix(daily$ITEM4[7:437],ncol = 1)
item4_p_te<-item4[432:618,]
item4_o_te<-daily$ITEM4[438:624]



nnetfit_item4<-nnet(item4_p_tr, item4_o_tr,
                    size= 5,
                    decay = 0.01,
                    linout = TRUE,
                    trace = FALSE,
                    maxit = 500,
                    MaxNWts = 5*(ncol(item4_p_tr)+1)+5+1
)
item4_ann<-as.data.frame(matrix(item4_o_tr[431:426],ncol = 6))
item4_ann_pred<-NULL
for (i in 1:187) {
  a<-predict(nnetfit_item4,item4_ann[i,])
  item4_ann_pred<-append(item4_ann_pred,a)
  item4_ann[i+1,]<-c(a,as.numeric(item4_ann[i,1:5]))
}

rmse(daily$ITEM4[438:624],item4_ann_pred)
mae(daily$ITEM4[438:624],item4_ann_pred)


### item5 ar6

item5_p_tr<-item5[1:431,]
item5_o_tr<-matrix(daily$ITEM5[7:437],ncol = 1)
item5_p_te<-item5[432:618,]
item5_o_te<-daily$ITEM5[438:624]



nnetfit_item5<-nnet(item5_p_tr, item5_o_tr,
                    size= 5,
                    decay = 0.01,
                    linout = TRUE,
                    trace = FALSE,
                    maxit = 500,
                    MaxNWts = 5*(ncol(item5_p_tr)+1)+5+1
)
item5_ann<-as.data.frame(matrix(item5_o_tr[431:426],ncol = 6))
item5_ann_pred<-NULL
for (i in 1:187) {
  a<-predict(nnetfit_item5,item5_ann[i,])
  item5_ann_pred<-append(item5_ann_pred,a)
  item5_ann[i+1,]<-c(a,as.numeric(item5_ann[i,1:5]))
}

rmse(daily$ITEM5[438:624],item5_ann_pred)
mae(daily$ITEM5[438:624],item5_ann_pred)

### item6 ar6

item6_p_tr<-item6[1:431,]
item6_o_tr<-matrix(daily$ITEM6[7:437],ncol = 1)
item6_p_te<-item6[432:618,]
item6_o_te<-daily$ITEM6[438:624]



nnetfit_item6<-nnet(item6_p_tr, item6_o_tr,
                    size= 5,
                    decay = 0.01,
                    linout = TRUE,
                    trace = FALSE,
                    maxit = 500,
                    MaxNWts = 5*(ncol(item6_p_tr)+1)+5+1
)
item6_ann<-as.data.frame(matrix(item6_o_tr[431:426],ncol = 6))
item6_ann_pred<-NULL
for (i in 1:187) {
  a<-predict(nnetfit_item6,item6_ann[i,])
  item6_ann_pred<-append(item6_ann_pred,a)
  item6_ann[i+1,]<-c(a,as.numeric(item6_ann[i,1:5]))
}

rmse(daily$ITEM6[438:624],item6_ann_pred)
mae(daily$ITEM6[438:624],item6_ann_pred)




#######hybrid-arima

### item1
item1_ml<-arima(daily$ITEM1[1:437],c(1,0,2),method = "ML")
item1_css<-arima(daily$ITEM1[1:437],c(2,0,1),method = "CSS")
item1_ml_re<-madf(item1_ml$residuals)
item1_css_re<-madf(item1_css$residuals)
item1_ml_re_o<-matrix(item1_ml$residuals[7:437],ncol = 1)
item1_css_re_o<-matrix(item1_css$residuals[7:437],ncol = 1)


nnetfit_item1_reml<-nnet(item1_ml_re, item1_ml_re_o,
                    size= 5,
                    decay = 0.01,
                    linout = TRUE,
                    trace = FALSE,
                    maxit = 500,
                    MaxNWts = 5*(ncol(item1_ml_re)+1)+5+1
)


item1_ml_re_t1<-as.data.frame(matrix(item1_ml_re_o[431:426],ncol = 6))
item1_ml_re_pred<-NULL
for (i in 1:187) {
  a<-predict(nnetfit_item1_reml,item1_ml_re_t1[i,])
  item1_ml_re_pred<-append(item1_ml_re_pred,a)
  item1_ml_re_t1[i+1,]<-c(a,as.numeric(item1_ml_re_t1[i,1:5]))
}
rmse(daily$ITEM1[438:624],predict(item1_ml,187)$pred+item1_ml_re_pred)
mae(daily$ITEM1[438:624],predict(item1_ml,187)$pred+item1_ml_re_pred)




nnetfit_item1_recss<-nnet(item1_css_re, item1_css_re_o,
                         size= 5,
                         decay = 0.01,
                         linout = TRUE,
                         trace = FALSE,
                         maxit = 500,
                         MaxNWts = 5*(ncol(item1_css_re)+1)+5+1
)


item1_css_re_t1<-as.data.frame(matrix(item1_css_re_o[431:426],ncol = 6))
item1_css_re_pred<-NULL
for (i in 1:187) {
  a<-predict(nnetfit_item1_recss,item1_css_re_t1[i,])
  item1_css_re_pred<-append(item1_css_re_pred,a)
  item1_css_re_t1[i+1,]<-c(a,as.numeric(item1_css_re_t1[i,1:5]))
}
rmse(daily$ITEM1[438:624],predict(item1_css,187)$pred+item1_css_re_pred)
mae(daily$ITEM1[438:624],predict(item1_css,187)$pred+item1_css_re_pred)






###item2 
### item1
item2_ml<-arima(daily$ITEM2[1:437],c(2,0,2),method = "ML")
item2_css<-arima(daily$ITEM2[1:437],c(2,0,2),method = "CSS")
item2_ml_re<-madf(item2_ml$residuals)
item2_css_re<-madf(item2_css$residuals)
item2_ml_re_o<-matrix(item2_ml$residuals[7:437],ncol = 1)
item2_css_re_o<-matrix(item2_css$residuals[7:437],ncol = 1)


nnetfit_item2_reml<-nnet(item2_ml_re, item2_ml_re_o,
                         size= 5,
                         decay = 0.01,
                         linout = TRUE,
                         trace = FALSE,
                         maxit = 500,
                         MaxNWts = 5*(ncol(item2_ml_re)+1)+5+1
)


item2_ml_re_t1<-as.data.frame(matrix(item2_ml_re_o[431:426],ncol = 6))
item2_ml_re_pred<-NULL
for (i in 1:187) {
  a<-predict(nnetfit_item2_reml,item2_ml_re_t1[i,])
  item2_ml_re_pred<-append(item2_ml_re_pred,a)
  item2_ml_re_t1[i+1,]<-c(a,as.numeric(item2_ml_re_t1[i,1:5]))
}
rmse(daily$ITEM2[438:624],predict(item2_ml,187)$pred+item2_ml_re_pred)
mae(daily$ITEM2[438:624],predict(item2_ml,187)$pred+item2_ml_re_pred)




nnetfit_item2_recss<-nnet(item2_css_re, item2_css_re_o,
                          size= 5,
                          decay = 0.01,
                          linout = TRUE,
                          trace = FALSE,
                          maxit = 500,
                          MaxNWts = 5*(ncol(item2_css_re)+1)+5+1
)


item2_css_re_t1<-as.data.frame(matrix(item2_css_re_o[431:426],ncol = 6))
item2_css_re_pred<-NULL
for (i in 1:187) {
  a<-predict(nnetfit_item2_recss,item2_css_re_t1[i,])
  item2_css_re_pred<-append(item2_css_re_pred,a)
  item2_css_re_t1[i+1,]<-c(a,as.numeric(item2_css_re_t1[i,1:5]))
}
rmse(daily$ITEM2[438:624],predict(item2_css,187)$pred+item2_css_re_pred)
mae(daily$ITEM2[438:624],predict(item2_css,187)$pred+item2_css_re_pred)


### item3
item3_ml<-arima(daily$ITEM3[1:437],c(2,2,1),method = "ML")
item3_css<-arima(daily$ITEM3[1:437],c(2,2,1),method = "CSS")
item3_ml_re<-madf(item3_ml$residuals)
item3_css_re<-madf(item3_css$residuals)
item3_ml_re_o<-matrix(item3_ml$residuals[7:437],ncol = 1)
item3_css_re_o<-matrix(item3_css$residuals[7:437],ncol = 1)


nnetfit_item3_reml<-nnet(item3_ml_re, item3_ml_re_o,
                         size= 5,
                         decay = 0.01,
                         linout = TRUE,
                         trace = FALSE,
                         maxit = 500,
                         MaxNWts = 5*(ncol(item3_ml_re)+1)+5+1
)


item3_ml_re_t1<-as.data.frame(matrix(item3_ml_re_o[431:426],ncol = 6))
item3_ml_re_pred<-NULL
for (i in 1:187) {
  a<-predict(nnetfit_item3_reml,item3_ml_re_t1[i,])
  item3_ml_re_pred<-append(item3_ml_re_pred,a)
  item3_ml_re_t1[i+1,]<-c(a,as.numeric(item3_ml_re_t1[i,1:5]))
}
rmse(daily$ITEM3[438:624],predict(item3_ml,187)$pred+item3_ml_re_pred)
mae(daily$ITEM3[438:624],predict(item3_ml,187)$pred+item3_ml_re_pred)




nnetfit_item3_recss<-nnet(item3_css_re, item3_css_re_o,
                          size= 5,
                          decay = 0.01,
                          linout = TRUE,
                          trace = FALSE,
                          maxit = 500,
                          MaxNWts = 5*(ncol(item3_css_re)+1)+5+1
)


item3_css_re_t1<-as.data.frame(matrix(item3_css_re_o[431:426],ncol = 6))
item3_css_re_pred<-NULL
for (i in 1:187) {
  a<-predict(nnetfit_item3_recss,item3_css_re_t1[i,])
  item3_css_re_pred<-append(item3_css_re_pred,a)
  item3_css_re_t1[i+1,]<-c(a,as.numeric(item3_css_re_t1[i,1:5]))
}
rmse(daily$ITEM3[438:624],predict(item3_css,187)$pred+item3_css_re_pred)
mae(daily$ITEM3[438:624],predict(item3_css,187)$pred+item3_css_re_pred)





### item4
item4_ml<-arima(daily$ITEM4[1:437],c(2,1,2),method = "ML")
item4_css<-arima(daily$ITEM4[1:437],c(2,0,1),method = "CSS")
item4_ml_re<-madf(item4_ml$residuals)
item4_css_re<-madf(item4_css$residuals)
item4_ml_re_o<-matrix(item4_ml$residuals[7:437],ncol = 1)
item4_css_re_o<-matrix(item4_css$residuals[7:437],ncol = 1)


nnetfit_item4_reml<-nnet(item4_ml_re, item4_ml_re_o,
                         size= 5,
                         decay = 0.01,
                         linout = TRUE,
                         trace = FALSE,
                         maxit = 500,
                         MaxNWts = 5*(ncol(item4_ml_re)+1)+5+1
)


item4_ml_re_t1<-as.data.frame(matrix(item4_ml_re_o[431:426],ncol = 6))
item4_ml_re_pred<-NULL
for (i in 1:187) {
  a<-predict(nnetfit_item4_reml,item4_ml_re_t1[i,])
  item4_ml_re_pred<-append(item4_ml_re_pred,a)
  item4_ml_re_t1[i+1,]<-c(a,as.numeric(item4_ml_re_t1[i,1:5]))
}
rmse(daily$ITEM4[438:624],predict(item4_ml,187)$pred+item4_ml_re_pred)
mae(daily$ITEM4[438:624],predict(item4_ml,187)$pred+item4_ml_re_pred)




nnetfit_item4_recss<-nnet(item4_css_re, item4_css_re_o,
                          size= 5,
                          decay = 0.01,
                          linout = TRUE,
                          trace = FALSE,
                          maxit = 500,
                          MaxNWts = 5*(ncol(item4_css_re)+1)+5+1
)


item4_css_re_t1<-as.data.frame(matrix(item4_css_re_o[431:426],ncol = 6))
item4_css_re_pred<-NULL
for (i in 1:187) {
  a<-predict(nnetfit_item4_recss,item4_css_re_t1[i,])
  item4_css_re_pred<-append(item4_css_re_pred,a)
  item4_css_re_t1[i+1,]<-c(a,as.numeric(item4_css_re_t1[i,1:5]))
}
rmse(daily$ITEM4[438:624],predict(item4_css,187)$pred+item4_css_re_pred)
mae(daily$ITEM4[438:624],predict(item4_css,187)$pred+item4_css_re_pred)






### item5
item5_ml<-arima(daily$ITEM5[1:437],c(2,1,1),method = "ML")
item5_css<-arima(daily$ITEM5[1:437],c(1,0,1),method = "CSS")
item5_ml_re<-madf(item5_ml$residuals)
item5_css_re<-madf(item5_css$residuals)
item5_ml_re_o<-matrix(item5_ml$residuals[7:437],ncol = 1)
item5_css_re_o<-matrix(item5_css$residuals[7:437],ncol = 1)


nnetfit_item5_reml<-nnet(item5_ml_re, item5_ml_re_o,
                         size= 5,
                         decay = 0.01,
                         linout = TRUE,
                         trace = FALSE,
                         maxit = 500,
                         MaxNWts = 5*(ncol(item5_ml_re)+1)+5+1
)


item5_ml_re_t1<-as.data.frame(matrix(item5_ml_re_o[431:426],ncol = 6))
item5_ml_re_pred<-NULL
for (i in 1:187) {
  a<-predict(nnetfit_item5_reml,item5_ml_re_t1[i,])
  item5_ml_re_pred<-append(item5_ml_re_pred,a)
  item5_ml_re_t1[i+1,]<-c(a,as.numeric(item5_ml_re_t1[i,1:5]))
}
rmse(daily$ITEM5[438:624],predict(item5_ml,187)$pred+item5_ml_re_pred)
mae(daily$ITEM5[438:624],predict(item5_ml,187)$pred+item5_ml_re_pred)




nnetfit_item5_recss<-nnet(item5_css_re, item5_css_re_o,
                          size= 5,
                          decay = 0.01,
                          linout = TRUE,
                          trace = FALSE,
                          maxit = 500,
                          MaxNWts = 5*(ncol(item5_css_re)+1)+5+1
)


item5_css_re_t1<-as.data.frame(matrix(item5_css_re_o[431:426],ncol = 6))
item5_css_re_pred<-NULL
for (i in 1:187) {
  a<-predict(nnetfit_item5_recss,item5_css_re_t1[i,])
  item5_css_re_pred<-append(item5_css_re_pred,a)
  item5_css_re_t1[i+1,]<-c(a,as.numeric(item5_css_re_t1[i,1:5]))
}
rmse(daily$ITEM5[438:624],predict(item5_css,187)$pred+item5_css_re_pred)
mae(daily$ITEM5[438:624],predict(item5_css,187)$pred+item5_css_re_pred)




### item6
item6_ml<-arima(daily$ITEM6[1:437],c(1,2,1),method = "ML")
item6_css<-arima(daily$ITEM6[1:437],c(1,2,2),method = "CSS")
item6_ml_re<-madf(item6_ml$residuals)
item6_css_re<-madf(item6_css$residuals)
item6_ml_re_o<-matrix(item6_ml$residuals[7:437],ncol = 1)
item6_css_re_o<-matrix(item6_css$residuals[7:437],ncol = 1)


nnetfit_item6_reml<-nnet(item6_ml_re, item6_ml_re_o,
                         size= 5,
                         decay = 0.01,
                         linout = TRUE,
                         trace = FALSE,
                         maxit = 500,
                         MaxNWts = 5*(ncol(item6_ml_re)+1)+5+1
)


item6_ml_re_t1<-as.data.frame(matrix(item6_ml_re_o[431:426],ncol = 6))
item6_ml_re_pred<-NULL
for (i in 1:187) {
  a<-predict(nnetfit_item6_reml,item6_ml_re_t1[i,])
  item6_ml_re_pred<-append(item6_ml_re_pred,a)
  item6_ml_re_t1[i+1,]<-c(a,as.numeric(item6_ml_re_t1[i,1:5]))
}
rmse(daily$ITEM6[438:624],predict(item6_ml,187)$pred+item6_ml_re_pred)
mae(daily$ITEM6[438:624],predict(item6_ml,187)$pred+item6_ml_re_pred)




nnetfit_item6_recss<-nnet(item6_css_re, item6_css_re_o,
                          size= 5,
                          decay = 0.01,
                          linout = TRUE,
                          trace = FALSE,
                          maxit = 500,
                          MaxNWts = 5*(ncol(item6_css_re)+1)+5+1
)


item6_css_re_t1<-as.data.frame(matrix(item6_css_re_o[431:426],ncol = 6))
item6_css_re_pred<-NULL
for (i in 1:187) {
  a<-predict(nnetfit_item6_recss,item6_css_re_t1[i,])
  item6_css_re_pred<-append(item6_css_re_pred,a)
  item6_css_re_t1[i+1,]<-c(a,as.numeric(item6_css_re_t1[i,1:5]))
}
rmse(daily$ITEM6[438:624],predict(item6_css,187)$pred+item6_css_re_pred)
mae(daily$ITEM6[438:624],predict(item6_css,187)$pred+item6_css_re_pred)




# GLS 
#item1
item1ann_resid<-daily$ITEM1[7:437]-predict(nnetfit_item1,item1_p_tr)
item1_res_ann<-lm(dyn(zoo(log(item1ann_resid^2))~lag(daily_zoo$ITEM1[7:437],-1)+lag(daily_zoo$ITEM1[7:437],-2)+lag(daily_zoo$ITEM1[7:437],-3)+
               lag(daily_zoo$ITEM1[7:437],-4)+lag(daily_zoo$ITEM1[7:437],-5)+lag(daily_zoo$ITEM1[7:437],-6)))
item1_res_ols<-lm(dyn(zoo(log(item1_ar6$residuals^2))~lag(daily_zoo$ITEM1[7:437],-1)+lag(daily_zoo$ITEM1[7:437],-2)+lag(daily_zoo$ITEM1[7:437],-3)+
                lag(daily_zoo$ITEM1[7:437],-4)+lag(daily_zoo$ITEM1[7:437],-5)+lag(daily_zoo$ITEM1[7:437],-6)))
item1_fgls<-lm(dyn(daily_zoo$ITEM1[7:437]~lag(daily_zoo$ITEM1[7:437],-1)+lag(daily_zoo$ITEM1[7:437],-2)+lag(daily_zoo$ITEM1[7:437],-3)+
                   lag(daily_zoo$ITEM1[7:437],-4)+lag(daily_zoo$ITEM1[7:437],-5)+lag(daily_zoo$ITEM1[7:437],-6)),weights=1/exp(c(rep(NA,12),fitted(item1_res_ann))))
item1_fgls2<-lm(dyn(daily_zoo$ITEM1[7:437]~lag(daily_zoo$ITEM1[7:437],-1)+lag(daily_zoo$ITEM1[7:437],-2)+lag(daily_zoo$ITEM1[7:437],-3)+
                   lag(daily_zoo$ITEM1[7:437],-4)+lag(daily_zoo$ITEM1[7:437],-5)+lag(daily_zoo$ITEM1[7:437],-6)),weights=zoo(1/exp(c(rep(NA,12),fitted(item1_res_ols)))))

item1_fgls

test_pred<-daily$ITEM1[432:437]

for (i in 1:187) {
  t<-24.620125-0.034356*test_pred[i+5]+0.009272*test_pred[i+4]-0.120208*test_pred[i+3]+
    0.083081*test_pred[i+2]+0.040296*test_pred[i+1]+0.311601*test_pred[i]
  test_pred<-append(test_pred,t)
}

rmse(daily$ITEM1[438:624],test_pred[7:193])
mae(daily$ITEM1[438:624],test_pred[7:193])


#item2
ITEM2_ar6<-item2_ar6
ITEM2ann_resid<-daily$ITEM2[7:437]-predict(nnetfit_item2,item2_p_tr)
ITEM2_res_ann<-lm(dyn(zoo(log(ITEM2ann_resid^2))~lag(daily_zoo$ITEM2[7:437],-1)+lag(daily_zoo$ITEM2[7:437],-2)+lag(daily_zoo$ITEM2[7:437],-3)+
                        lag(daily_zoo$ITEM2[7:437],-4)+lag(daily_zoo$ITEM2[7:437],-5)+lag(daily_zoo$ITEM2[7:437],-6)))
ITEM2_res_ols<-lm(dyn(zoo(log(ITEM2_ar6$residuals^2))~lag(daily_zoo$ITEM2[7:437],-1)+lag(daily_zoo$ITEM2[7:437],-2)+lag(daily_zoo$ITEM2[7:437],-3)+
                        lag(daily_zoo$ITEM2[7:437],-4)+lag(daily_zoo$ITEM2[7:437],-5)+lag(daily_zoo$ITEM2[7:437],-6)))
ITEM2_fgls<-lm(dyn(daily_zoo$ITEM2[7:437]~lag(daily_zoo$ITEM2[7:437],-1)+lag(daily_zoo$ITEM2[7:437],-2)+lag(daily_zoo$ITEM2[7:437],-3)+
                     lag(daily_zoo$ITEM2[7:437],-4)+lag(daily_zoo$ITEM2[7:437],-5)+lag(daily_zoo$ITEM2[7:437],-6)),weights=1/exp(c(rep(NA,12),fitted(ITEM2_res_ann))))
ITEM2_fgls2<-lm(dyn(daily_zoo$ITEM2[7:437]~lag(daily_zoo$ITEM2[7:437],-1)+lag(daily_zoo$ITEM2[7:437],-2)+lag(daily_zoo$ITEM2[7:437],-3)+
                      lag(daily_zoo$ITEM2[7:437],-4)+lag(daily_zoo$ITEM2[7:437],-5)+lag(daily_zoo$ITEM2[7:437],-6)),weights=zoo(1/exp(c(rep(NA,12),fitted(ITEM2_res_ols)))))

ITEM2_fgls2$coefficients[[1]]

test_pred<-daily$ITEM2[432:437]

for (i in 1:187) {
  t<-ITEM2_fgls2$coefficients[[1]]+ITEM2_fgls2$coefficients[[2]]*test_pred[i+5]+
    ITEM2_fgls2$coefficients[[3]]*test_pred[i+4]+ITEM2_fgls2$coefficients[[4]]*test_pred[i+3]+
    ITEM2_fgls2$coefficients[[5]]*test_pred[i+2]+ITEM2_fgls2$coefficients[[6]]*test_pred[i+1]+
    ITEM2_fgls2$coefficients[[7]]*test_pred[i]
  test_pred<-append(test_pred,t)
}

rmse(daily$ITEM2[438:624],test_pred[7:193])
mae(daily$ITEM2[438:624],test_pred[7:193])




#item3
ITEM3_ar6<-item3_ar6
ITEM3ann_resid<-daily$ITEM3[7:437]-predict(nnetfit_item3,item3_p_tr)
ITEM3_res_ann<-lm(dyn(zoo(log(ITEM3ann_resid^2))~lag(daily_zoo$ITEM3[7:437],-1)+lag(daily_zoo$ITEM3[7:437],-2)+lag(daily_zoo$ITEM3[7:437],-3)+
                        lag(daily_zoo$ITEM3[7:437],-4)+lag(daily_zoo$ITEM3[7:437],-5)+lag(daily_zoo$ITEM3[7:437],-6)))
ITEM3_res_ols<-lm(dyn(zoo(log(ITEM3_ar6$residuals^2))~lag(daily_zoo$ITEM3[7:437],-1)+lag(daily_zoo$ITEM3[7:437],-2)+lag(daily_zoo$ITEM3[7:437],-3)+
                        lag(daily_zoo$ITEM3[7:437],-4)+lag(daily_zoo$ITEM3[7:437],-5)+lag(daily_zoo$ITEM3[7:437],-6)))
ITEM3_fgls<-lm(dyn(daily_zoo$ITEM3[7:437]~lag(daily_zoo$ITEM3[7:437],-1)+lag(daily_zoo$ITEM3[7:437],-2)+lag(daily_zoo$ITEM3[7:437],-3)+
                     lag(daily_zoo$ITEM3[7:437],-4)+lag(daily_zoo$ITEM3[7:437],-5)+lag(daily_zoo$ITEM3[7:437],-6)),weights=1/exp(c(rep(NA,12),fitted(ITEM3_res_ann))))
ITEM3_fgls2<-lm(dyn(daily_zoo$ITEM3[7:437]~lag(daily_zoo$ITEM3[7:437],-1)+lag(daily_zoo$ITEM3[7:437],-2)+lag(daily_zoo$ITEM3[7:437],-3)+
                      lag(daily_zoo$ITEM3[7:437],-4)+lag(daily_zoo$ITEM3[7:437],-5)+lag(daily_zoo$ITEM3[7:437],-6)),weights=zoo(1/exp(c(rep(NA,12),fitted(ITEM3_res_ols)))))



test_pred<-daily$ITEM3[432:437]

for (i in 1:187) {
  t<-ITEM3_fgls$coefficients[[1]]+ITEM3_fgls$coefficients[[2]]*test_pred[i+5]+
    ITEM3_fgls$coefficients[[3]]*test_pred[i+4]+ITEM3_fgls$coefficients[[4]]*test_pred[i+3]+
    ITEM3_fgls$coefficients[[5]]*test_pred[i+2]+ITEM3_fgls$coefficients[[6]]*test_pred[i+1]+
    ITEM3_fgls$coefficients[[7]]*test_pred[i]
  test_pred<-append(test_pred,t)
}

rmse(daily$ITEM3[438:624],test_pred[7:193])
mae(daily$ITEM3[438:624],test_pred[7:193])



test_pred<-daily$ITEM3[432:437]

for (i in 1:187) {
  t<-ITEM3_fgls2$coefficients[[1]]+ITEM3_fgls2$coefficients[[2]]*test_pred[i+5]+
    ITEM3_fgls2$coefficients[[3]]*test_pred[i+4]+ITEM3_fgls2$coefficients[[4]]*test_pred[i+3]+
    ITEM3_fgls2$coefficients[[5]]*test_pred[i+2]+ITEM3_fgls2$coefficients[[6]]*test_pred[i+1]+
    ITEM3_fgls2$coefficients[[7]]*test_pred[i]
  test_pred<-append(test_pred,t)
}

rmse(daily$ITEM3[438:624],test_pred[7:193])
mae(daily$ITEM3[438:624],test_pred[7:193])


#item4
ITEM4_ar6<-item4_ar6
ITEM4ann_resid<-daily$ITEM4[7:437]-predict(nnetfit_item4,item4_p_tr)
ITEM4_res_ann<-lm(dyn(zoo(log(ITEM4ann_resid^2))~lag(daily_zoo$ITEM4[7:437],-1)+lag(daily_zoo$ITEM4[7:437],-2)+lag(daily_zoo$ITEM4[7:437],-3)+
                        lag(daily_zoo$ITEM4[7:437],-4)+lag(daily_zoo$ITEM4[7:437],-5)+lag(daily_zoo$ITEM4[7:437],-6)))
ITEM4_res_ols<-lm(dyn(zoo(log(ITEM4_ar6$residuals^2))~lag(daily_zoo$ITEM4[7:437],-1)+lag(daily_zoo$ITEM4[7:437],-2)+lag(daily_zoo$ITEM4[7:437],-3)+
                        lag(daily_zoo$ITEM4[7:437],-4)+lag(daily_zoo$ITEM4[7:437],-5)+lag(daily_zoo$ITEM4[7:437],-6)))
ITEM4_fgls<-lm(dyn(daily_zoo$ITEM4[7:437]~lag(daily_zoo$ITEM4[7:437],-1)+lag(daily_zoo$ITEM4[7:437],-2)+lag(daily_zoo$ITEM4[7:437],-3)+
                     lag(daily_zoo$ITEM4[7:437],-4)+lag(daily_zoo$ITEM4[7:437],-5)+lag(daily_zoo$ITEM4[7:437],-6)),weights=1/exp(c(rep(NA,12),fitted(ITEM4_res_ann))))
ITEM4_fgls2<-lm(dyn(daily_zoo$ITEM4[7:437]~lag(daily_zoo$ITEM4[7:437],-1)+lag(daily_zoo$ITEM4[7:437],-2)+lag(daily_zoo$ITEM4[7:437],-3)+
                      lag(daily_zoo$ITEM4[7:437],-4)+lag(daily_zoo$ITEM4[7:437],-5)+lag(daily_zoo$ITEM4[7:437],-6)),weights=zoo(1/exp(c(rep(NA,12),fitted(ITEM4_res_ols)))))



test_pred<-daily$ITEM4[432:437]

for (i in 1:187) {
  t<-ITEM4_fgls$coefficients[[1]]+ITEM4_fgls$coefficients[[2]]*test_pred[i+5]+
    ITEM4_fgls$coefficients[[3]]*test_pred[i+4]+ITEM4_fgls$coefficients[[4]]*test_pred[i+3]+
    ITEM4_fgls$coefficients[[5]]*test_pred[i+2]+ITEM4_fgls$coefficients[[6]]*test_pred[i+1]+
    ITEM4_fgls$coefficients[[7]]*test_pred[i]
  test_pred<-append(test_pred,t)
}

rmse(daily$ITEM4[438:624],test_pred[7:193])
mae(daily$ITEM4[438:624],test_pred[7:193])



test_pred<-daily$ITEM4[432:437]

for (i in 1:187) {
  t<-ITEM4_fgls2$coefficients[[1]]+ITEM4_fgls2$coefficients[[2]]*test_pred[i+5]+
    ITEM4_fgls2$coefficients[[3]]*test_pred[i+4]+ITEM4_fgls2$coefficients[[4]]*test_pred[i+3]+
    ITEM4_fgls2$coefficients[[5]]*test_pred[i+2]+ITEM4_fgls2$coefficients[[6]]*test_pred[i+1]+
    ITEM4_fgls2$coefficients[[7]]*test_pred[i]
  test_pred<-append(test_pred,t)
}

rmse(daily$ITEM4[438:624],test_pred[7:193])
mae(daily$ITEM4[438:624],test_pred[7:193])



#item5
ITEM5_ar6<-item5_ar6
ITEM5ann_resid<-daily$ITEM4[7:437]-predict(nnetfit_item5,item5_p_tr)
ITEM5_res_ann<-lm(dyn(zoo(log(ITEM5ann_resid^2))~lag(daily_zoo$ITEM5[7:437],-1)+lag(daily_zoo$ITEM5[7:437],-2)+lag(daily_zoo$ITEM5[7:437],-3)+
                        lag(daily_zoo$ITEM5[7:437],-4)+lag(daily_zoo$ITEM5[7:437],-5)+lag(daily_zoo$ITEM5[7:437],-6)))
ITEM5_res_ols<-lm(dyn(zoo(log(ITEM5_ar6$residuals^2))~lag(daily_zoo$ITEM5[7:437],-1)+lag(daily_zoo$ITEM5[7:437],-2)+lag(daily_zoo$ITEM5[7:437],-3)+
                        lag(daily_zoo$ITEM5[7:437],-4)+lag(daily_zoo$ITEM5[7:437],-5)+lag(daily_zoo$ITEM5[7:437],-6)))
ITEM5_fgls<-lm(dyn(daily_zoo$ITEM5[7:437]~lag(daily_zoo$ITEM5[7:437],-1)+lag(daily_zoo$ITEM5[7:437],-2)+lag(daily_zoo$ITEM5[7:437],-3)+
                     lag(daily_zoo$ITEM5[7:437],-4)+lag(daily_zoo$ITEM5[7:437],-5)+lag(daily_zoo$ITEM5[7:437],-6)),weights=1/exp(c(rep(NA,12),fitted(ITEM5_res_ann))))
ITEM5_fgls2<-lm(dyn(daily_zoo$ITEM5[7:437]~lag(daily_zoo$ITEM5[7:437],-1)+lag(daily_zoo$ITEM5[7:437],-2)+lag(daily_zoo$ITEM5[7:437],-3)+
                      lag(daily_zoo$ITEM5[7:437],-4)+lag(daily_zoo$ITEM5[7:437],-5)+lag(daily_zoo$ITEM5[7:437],-6)),weights=zoo(1/exp(c(rep(NA,12),fitted(ITEM5_res_ols)))))



test_pred<-daily$ITEM5[432:437]

for (i in 1:187) {
  t<-ITEM5_fgls$coefficients[[1]]+ITEM5_fgls$coefficients[[2]]*test_pred[i+5]+
    ITEM5_fgls$coefficients[[3]]*test_pred[i+4]+ITEM5_fgls$coefficients[[4]]*test_pred[i+3]+
    ITEM5_fgls$coefficients[[5]]*test_pred[i+2]+ITEM5_fgls$coefficients[[6]]*test_pred[i+1]+
    ITEM5_fgls$coefficients[[7]]*test_pred[i]
  test_pred<-append(test_pred,t)
}

rmse(daily$ITEM5[438:624],test_pred[7:193])
mae(daily$ITEM5[438:624],test_pred[7:193])



test_pred<-daily$ITEM5[432:437]

for (i in 1:187) {
  t<-ITEM5_fgls2$coefficients[[1]]+ITEM5_fgls2$coefficients[[2]]*test_pred[i+5]+
    ITEM5_fgls2$coefficients[[3]]*test_pred[i+4]+ITEM5_fgls2$coefficients[[4]]*test_pred[i+3]+
    ITEM5_fgls2$coefficients[[5]]*test_pred[i+2]+ITEM5_fgls2$coefficients[[6]]*test_pred[i+1]+
    ITEM5_fgls2$coefficients[[7]]*test_pred[i]
  test_pred<-append(test_pred,t)
}

rmse(daily$ITEM5[438:624],test_pred[7:193])
mae(daily$ITEM5[438:624],test_pred[7:193])



#item6
ITEM6_ar6<-item6_ar6
ITEM6ann_resid<-daily$ITEM4[7:437]-predict(nnetfit_item6,item6_p_tr)
ITEM6_res_ann<-lm(dyn(zoo(log(ITEM6ann_resid^2))~lag(daily_zoo$ITEM6[7:437],-1)+lag(daily_zoo$ITEM6[7:437],-2)+lag(daily_zoo$ITEM6[7:437],-3)+
                        lag(daily_zoo$ITEM6[7:437],-4)+lag(daily_zoo$ITEM6[7:437],-5)+lag(daily_zoo$ITEM6[7:437],-6)))
ITEM6_res_ols<-lm(dyn(zoo(log(ITEM6_ar6$residuals^2))~lag(daily_zoo$ITEM6[7:437],-1)+lag(daily_zoo$ITEM6[7:437],-2)+lag(daily_zoo$ITEM6[7:437],-3)+
                        lag(daily_zoo$ITEM6[7:437],-4)+lag(daily_zoo$ITEM6[7:437],-5)+lag(daily_zoo$ITEM6[7:437],-6)))
ITEM6_fgls<-lm(dyn(daily_zoo$ITEM6[7:437]~lag(daily_zoo$ITEM6[7:437],-1)+lag(daily_zoo$ITEM6[7:437],-2)+lag(daily_zoo$ITEM6[7:437],-3)+
                     lag(daily_zoo$ITEM6[7:437],-4)+lag(daily_zoo$ITEM6[7:437],-5)+lag(daily_zoo$ITEM6[7:437],-6)),weights=1/exp(c(rep(NA,12),fitted(ITEM6_res_ann))))
ITEM6_fgls2<-lm(dyn(daily_zoo$ITEM6[7:437]~lag(daily_zoo$ITEM6[7:437],-1)+lag(daily_zoo$ITEM6[7:437],-2)+lag(daily_zoo$ITEM6[7:437],-3)+
                      lag(daily_zoo$ITEM6[7:437],-4)+lag(daily_zoo$ITEM6[7:437],-5)+lag(daily_zoo$ITEM6[7:437],-6)),weights=zoo(1/exp(c(rep(NA,12),fitted(ITEM6_res_ols)))))



test_pred<-daily$ITEM6[432:437]

for (i in 1:187) {
  t<-ITEM6_fgls$coefficients[[1]]+ITEM6_fgls$coefficients[[2]]*test_pred[i+5]+
    ITEM6_fgls$coefficients[[3]]*test_pred[i+4]+ITEM6_fgls$coefficients[[4]]*test_pred[i+3]+
    ITEM6_fgls$coefficients[[5]]*test_pred[i+2]+ITEM6_fgls$coefficients[[6]]*test_pred[i+1]+
    ITEM6_fgls$coefficients[[7]]*test_pred[i]
  test_pred<-append(test_pred,t)
}

rmse(daily$ITEM6[438:624],test_pred[7:193])
mae(daily$ITEM6[438:624],test_pred[7:193])



test_pred<-daily$ITEM6[432:437]

for (i in 1:187) {
  t<-ITEM6_fgls2$coefficients[[1]]+ITEM6_fgls2$coefficients[[2]]*test_pred[i+5]+
    ITEM6_fgls2$coefficients[[3]]*test_pred[i+4]+ITEM6_fgls2$coefficients[[4]]*test_pred[i+3]+
    ITEM6_fgls2$coefficients[[5]]*test_pred[i+2]+ITEM6_fgls2$coefficients[[6]]*test_pred[i+1]+
    ITEM6_fgls2$coefficients[[7]]*test_pred[i]
  test_pred<-append(test_pred,t)
}

rmse(daily$ITEM6[438:624],test_pred[7:193])
mae(daily$ITEM6[438:624],test_pred[7:193])


## 2019052101
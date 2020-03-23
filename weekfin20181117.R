################################################################
#### 1. install Packages ####
#getwd()
#install.packages("dbplyr")
#install.packages("tidyr")
#remove.packages("ROracle")
#install.packages("ROracle.zip",repos = NULL)
#install.packages("tseries")
#install.packages("forecast")
##########################################################################################
#### 2. Connection to Database ####
library(DBI)
library(dplyr);library(tidyr)
library(dbplyr)
library(ROracle)
library(forecast)
##########################################################################################
# drv: 오라클 서버를 이용하기 때문에 "Oracle" 사용
# host: 서버주소
# port: 포트
# sid: 서버이름
# connect.string에 위 내용을 포함한 string을 저장
# dbconnect를 통해서 db에 접속
Sys.setenv(ORA_SDTZ = "KST")
Sys.setenv(TZ = "KST")
drv<-dbDriver("Oracle")

host <- "주소"
port<-"포트"
sid<-"sid"
connect.string <- paste(
  "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
  "(CONNECT_DATA=(SID=", sid, ")))", sep = "")


con <- dbConnect(drv, username = "유저네임", password = "비번", 
                 dbname = connect.string, prefetch = FALSE,
                 bulk_read = 1000L, stmt_cache = 0L, 
                 external_credentials = FALSE, sysdba = FALSE)


week_info=dbGetQuery(con, "SELECT WEEK_CUNT 
                      ,STND_YEAR 
                     ,STND_WEEK 
                     FROM DMS_WEEKINFO
                     WHERE FROM_DATE <= TO_CHAR(SYSDATE+1,'YYYYMMDD') 
                     AND TOTO_DATE >= TO_CHAR(SYSDATE+1,'YYYYMMDD')")
workst <- function(){
  # 이 함수를 통해 DB가 업데이트 되었는지 확인할 수 있다.
  # 만약 예측을 진행할 수 있는 상황이면 Ready to work! 라는 표시가 뜬다.
  created <- dbGetQuery(con,paste0("SELECT FNSH_DATE FROM DM_IF_WORKINFO WHERE WEEK_CUNT =",
                                (week_info$WEEK_CUNT-1)))
  
  if(length(created$FNSH_DATE)>0){
    print("Ready to work")
  }
}


workst()

##########################################################################################
#### 3. read TABLE
# dm_if_mechmas에 모든 주문내역을 저장한다.
dm_if_mechmas <- dbGetQuery(con,"SELECT A.*
FROM DM_IF_MECHMAS_  A
WHERE A.INPU_GUBN = '1'")




##########################################################################################
#### 4. Create Time Serise Data ####
# dm_if_mechmas를 시계열 형태의 데이터 프레임으로 변환시키는 함수
# to_timeseries를 만든다.
to_timeseries <- function(x){
  # dplyr과 tidyr을 사용하여 함수를 만듬.
  require(dplyr)
  require(tidyr)
  Date <- unique(x$WEEK_CUNT)
  Date <- sort(Date)
  u_ic <- length(unique(x$ITEM_CODE))
  u_date <- length(unique(x$WEEK_CUNT))
  dd <- data.frame(matrix(data=rep(NA,(u_ic*u_date)),nrow=u_date))
  colnames(dd)<-unique(x$ITEM_CODE)
  for(i in unique(x$ITEM_CODE)){
    qq <- matrix(data=rep(NA,u_date),ncol=1)
    qq <- data.frame(Date,qq)
    colnames(qq) <- c("WEEK_CUNT","STCK_QNTY")
    qq2 <- subset(x[,c("WEEK_CUNT","STCK_QNTY")],x$ITEM_CODE==i)
    qq2 <- group_by(qq2,WEEK_CUNT)
    qq2 <- summarise(qq2,STCK_QNTY_=sum(STCK_QNTY))
    qq2 <- left_join(qq,qq2,by="WEEK_CUNT")
    dd[,i]<-qq2$STCK_QNTY_
    p <- which(!is.na(dd[,i]))[1]
    dd[,i][p:u_date][is.na(dd[,i][p:u_date])]<-0
    p2 <- which(!dd[,i]==0)[1]
    if(is.na(p2)){
      next
    } else if(p2>1){
      dd[,i][1:(p2-1)] <- NA
    }
  }
  return(dd[(length(Date)-103):length(Date),])
}
# mstime104에 to_timeseries에 dm_if_mechmas를 인수로 집어넣어 시계열
# 데이터 프레임을 만듦.
mstime104 <- to_timeseries(dm_if_mechmas)
##########################################################################################
#### 4. Fundamental Statistics
stat104<-function(x){
  require(forecast)
  item_code<-colnames(x)
  mean1<-NULL
  ARrmse<-NULL
  sample_c<-NULL
  for(p in item_code){
    t<-mean(x[,p],na.rm = T)
    mean1<-append(mean1,t)
    sample_c<-append(sample_c,sum(!is.na(x[,p])))
    tryCatch({
      model=arima(x[,p],order = c(1,0,0))
    }, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})
    t2<-fitted.values(model)
    rmse<-sqrt(mean((x[,p]-t2)^2,na.rm = T))
    ARrmse<-append(ARrmse,rmse)
  }
  df1<-data.frame(item_code,mean1,sample_c,ARrmse,stringsAsFactors = F)
  df1$rmse_mean<-df1$ARrmse/df1$mean1
  return(df1)
}
statweek<-stat104(mstime104)


##########################################################################################
#### 5. Classifier ####
# 1주별 데이터 Itemcode를 구하는 Classfyweeks라는 함수를 만듦.
classfyweeks <- function(x){
  x<-subset(x,x$sample_c>=24)
  weeks <- x$item_code[x$rmse_mean<0.7]
  weeks <- data.frame(weeks,stringsAsFactors = F)
  weeks$cycle <- rep("1",length(weeks))
  return(weeks)
}

# 2주별 데이터 Itemcode를 구하는 Classfy2weeks라는 함수를 만듦.
classfy2weeks <- function(x){
  x<-subset(x,x$sample_c>=24)
  weeks2 <- x$item_code[x$rmse_mean>=0.7&x$rmse_mean<1.4]
  weeks2 <- data.frame(weeks2,stringsAsFactors = F)
  weeks2$cycle <- rep("2",length(weeks2))
  return(weeks2)
}

# 4주별 데이터 Itemcode를 구하는 Classfy4weeks라는 함수를 만듦.

classfy4weeks<-function(x){
  x<-subset(x,x$sample_c>=24)
  weeks4<-x$item_code[x$rmse_mean>=1.4]
  weeks4<-data.frame(weeks4,stringsAsFactors = F)
  weeks4$cycle<-rep("3",length(weeks4))
  return(weeks4)
}

# 5주이상 24주 미만 데이터
classfyimp1<-function(x){
  x<-subset(x,x$sample_c>=5)
  x<-subset(x,x$sample_c<24)
  imp1<-x$item_code
  imp1<-data.frame(imp1,stringsAsFactors = F)
  imp1$cycle<-rep("4",length(imp1))
  return(imp1)
}

# 5주미만 데이터
classfyimp2<-function(x){
  x<-subset(x,x$sample_c<5)
  imp2<-x$item_code
  imp2<-data.frame(imp2,stringsAsFactors = F)
  imp2$cycle<-rep("5",length(imp2))
  return(imp2)
}

# weeks, wees_2, wees_4에 각각 1주별, 2주별, 4주별 itemcode를 할당함.
weeks <- classfyweeks(statweek)
weeks_2 <- classfy2weeks(statweek)
weeks_4 <- classfy4weeks(statweek)
imp1<-classfyimp1(statweek)
imp2<-classfyimp2(statweek)


##########################################################################################
#### 6. DM_IF_ITEMWORK Updating
# dbSendStatement와 DELETE 쿼리를 통해 DM_IF_ITEMWORK에 데이터를 삽입할 준비를 함.
# upitem이라는 데이터프레임을 만듦.
# upitem은 WEEK_CUNT와 ITEM_CODE와 CYCLE_GUBN, JOBS_GUBN, JOBS_DATE로 이루어져 있음.
# WEEK_CUNT에는 dm_if_mechmas의 WEEK_CUNT의 최댓값을 넣어줌으로 누적주차를 넣음.
# CYCL_GUBN과 ITEM_CODE에는 ITEM_CODE에 따라 크게 3가지 1(week), 2(half), 3(month)의 내용이 있음.
# 이렇게 만들어진 Table을 DM_IF_ITEMWORK에 집어넣음. db_commit으로 commit을 함.
dbSendQuery(con,paste0("DELETE FROM DM_IF_ITEMWORK WHERE WEEK_CUNT=",(week_info$WEEK_CUNT)))
upitem<-data.frame(matrix(rep(NA,length(statweek$item_code)*5),ncol=5))
colnames(upitem)<-c("WEEK_CUNT","ITEM_CODE","CYCL_GUBN","JOBS_GUBN","JOBS_DATE")
upitem$JOBS_DATE[1:length(statweek$item_code)]<-as.character(format(Sys.time(),"%Y-%m-%d"))
upitem$JOBS_DATE<-as.POSIXct(upitem$JOBS_DATE)
upitem$ITEM_CODE<-c(weeks$weeks,weeks_2$weeks2,weeks_4$weeks4,imp1$imp1,imp2$imp2)
upitem$WEEK_CUNT<-rep(max(week_info$WEEK_CUNT),length(statweek$item_code))
upitem$CYCL_GUBN<-c(weeks$cycle,weeks_2$cycle,weeks_4$cycle,imp1$cycle,imp2$cycle)
upitem$JOBS_GUBN<-rep("2",length(statweek$item_code))
dbWriteTable(con,"DM_IF_ITEMWORK",upitem,append=T)
db_commit(con)

aaa<-dbGetQuery(con,"SELECT * FROM DM_IF_ITEMWORK WHERE
                WEEK_CUNT = 411")

##########################################################################################
#### 7. Create Time Series(W,H,F) ####
# 1주별 데이터를 mstime_week에 할당
mstime_week <- mstime104[,weeks$weeks]

# 2주씩 묶어주는 함수를 to_timeseriesH에 할당.
to_timeseriesH<-function(x){
  ts2weeks<-NULL
  for( i in 1:52){
    a<-x[(2*i-1),]
    a2<-x[(2*i),]
    a[is.na(a)]<-0
    a2[is.na(a2)]<-0
    t<-a+a2
    ts2weeks<-rbind(ts2weeks,t)
  }
  for (i in colnames(ts2weeks)) {
    p<-which(!ts2weeks[i]==0)[1]
    if(is.na(p)){
      next
    } else if(p>1){
      ts2weeks[,i][1:(p-1)]<-NA
    }
  }
  return(ts2weeks)
}
# 2주별 데이터를 mstime_half에 할당.
mstime_half<-to_timeseriesH(mstime104[,weeks_2$weeks2])
# 4주씩 묶어주는 함수를 to_timeseriesM에 할당.
to_timeseriesM<-function(x){
  ts4weeks<-NULL
  for( i in 1:26){
    a<-x[(4*i-3),]
    a2<-x[(4*i-2),]
    a3<-x[(4*i-1),]
    a4<-x[(4*i),]
    a[is.na(a)]<-0
    a2[is.na(a2)]<-0
    a3[is.na(a3)]<-0
    a4[is.na(a4)]<-0
    t<-a+a2+a3+a4
    ts4weeks<-rbind(ts4weeks,t)
  }
  for (i in colnames(ts4weeks)) {
    p<-which(!ts4weeks[i]==0)[1]
    if(is.na(p)){
      next
    } else if(p>1){
      ts4weeks[,i][1:(p-1)]<-NA
    }
  }
  return(ts4weeks)
}
# 4주별 데이터를 mstime_month에 할당하였음.
mstime_month<-to_timeseriesM(mstime104[,weeks_4$weeks4])

##########################################################################################
#### 8. Information dataframe ####
# 시계열 예측을 조금 더 정확하게 하기 위해서 차분을 몇번할 것인지, 로그화를 할 것인지에 대한
# 함수를 info에 할당하였음.
info<-function(x){
  #logp에는 로그화를 할 것인지 안할 것인지에 대한 정보가 담겨있음. 1은 불가, 0은 가능을 의미함.
  # unit1에는 첫번째 유닛루트테스트의 p.value를 할당하였음.
  # unit2에는 두번째 유닛루트테스트의 p.value를 할당하였음.
  # 최종적으로 diff1에는 차분을 몇번할 것인지에 대한 정보를 unit1, unit2를 통하여 계산하였음.
  # item_code, logp, unit1, unit2, diff1으로 이루어진 함수를 반환함.
  require(tseries)
  item_code<-colnames(x)
  logp<-NULL
  unit1<-NULL
  unit2<-NULL
  for(i in item_code){
    logms<-log(x[,i])
    if (sum(is.infinite(logms))>0) {
      logp<-append(logp,1)
      logms<-x[,i]
    } else {
      logp<-append(logp,0)}
    tryCatch({
      p2<-adf.test(logms[!is.na(logms)])
    }, error = function(e){cat("ERROR :", conditionMessage(e),"\n")})
    unit1<-append(unit1,p2$p.value)
    tryCatch({
      p3<-adf.test(diff(logms[!is.na(logms)]))
    }, error = function(e){cat("ERROR :", conditionMessage(e),"\n")})
    unit2<-append(unit2,p3$p.value)
  }
  df1<-data.frame(item_code,logp,unit1,unit2,stringsAsFactors = F)
  df1$diff1<-rep(NA,length(item_code))
  df1$diff1[df1$unit1<0.05]<-0
  df1$diff1[is.na(df1$unit1)]<-0
  df1$diff1[df1$unit1>0.05 & df1$unit2<0.05]<-1
  df1$diff1[df1$unit1>0.05 & df1$unit2>0.05]<-2
  df1$diff1[is.na(df1$diff1)]<-0
  return(df1)
}
# info_all, info_w, info_m, info_h에 각각의 차분, 로그화에 대한 정보를 할당함.
info_all<-info(mstime104)
info_w<-info(mstime_week)
info_m<-info(mstime_month)
info_h<-info(mstime_half)
##########################################################################################
#### 9.Preparing Data ####
# 위에서 계산한 로그화에 대한 정보를 이용하여 시계열을 로그화 함.
mstime_week[,info_w$item_code[info_w$logp==0]]<-log(mstime_week[,info_w$item_code[info_w$logp==0]])
mstime_half[,info_h$item_code[info_h$logp==0]]<-log(mstime_half[,info_h$item_code[info_h$logp==0]])
mstime_month[,info_m$item_code[info_m$logp==0]]<-log(mstime_month[,info_m$item_code[info_m$logp==0]])

# sARIMA 모형을 찾기위한 순서쌍을 생성하기 위한 함수를 combination에 할당.
combination<-function(){
  df1 <- data.frame(matrix(rep(NA,324*5),ncol = 324))
  aaa <- c(0,1,2)
  aaa2 <- c(0,1,2,3)
  t <- 0
  for(i in aaa2){
    for(i2 in aaa){
      for(i3 in aaa){
        for(i4 in aaa){
          for(i5 in aaa){
            t <- t+1
            t1 <- c(i,i2,i3,i4,i5)
            df1[,t] <- t1
          }
        }
      }
    }
  }
  return(df1)
}
# com_에 sARIMA의 순서쌍을 할당하였음.
com_ <- combination()

#######################################################################################
#### 10. predict ####
# 위까지 왔을 때, 커넥션이 끊어지는 경우가 빈번하게 발생하여 다시한번 DB에 연결.
con <- dbConnect(drv, username = "MSFS_DM", password = "MSFSDM9", 
                 dbname = connect.string, prefetch = FALSE,
                 bulk_read = 1000L, stmt_cache = 0L, 
                 external_credentials = FALSE, sysdba = FALSE)

#######################################################################################
# Monthly data를 예측하는 predict_M이라는 함수를 만듦.
predict_M<-function(x){
  # forecast 패키지가 필요함.
  # 예측주차에 대한 정보를 할당함.
  # 순서쌍, 예측치, 누락 아이템, 반복횟수 등을 저장할 공간을 만듦.
  require(forecast)
  CYCL_GUBN<-3
  WEEK_CUNT<-week_info$WEEK_CUNT
  DMND_WEEK<-week_info$STND_WEEK
  DMND_YEAR<-week_info$STND_YEAR
  order1<-NULL
  fit1<-NULL
  predict1<-NULL
  item_code<-colnames(x)
  iter<-0
  f_item<-NULL
  for (i in item_code){
    # 반복횟수를 1씩 증가시키고 아이템코드와 반복횟수를 프린트함.
    # RMSE를 저장할 공간을 확보.
    iter<-iter+1
    print(i)
    print(iter)
    RMSE1<-rep(NA,324)
    for (i2 in 1:324) {
      # 각 순서쌍에 따라 324번 모형을 만들고 성능을 비교함.
      tryCatch({
        model = arima(x[i],order = c(com_[1,i2],info_m$diff1[info_m$item_code==i],com_[2,i2]),
                      seasonal = list(order=c(com_[3,i2],com_[4,i2],com_[5,i2]),period=4))
        fit1<-fitted.values(model)
        rmse<-sqrt(mean((x[,i]-fit1)^2,na.rm = T))
        RMSE1[i2]<-rmse
      }, error =function(e1){cat("ERROR :", conditionMessage(e1), "\n")})
    }
    # 위 324번 반복으로 최적의 순서쌍을 확보하고, 다시한번 모델을 구성하고 예측값을 구함.
    min_index = which.min(RMSE1)
    min_order = com_[,min_index]
    tryCatch({
      model2<-arima(x[i],order = c(min_order[1],info_m$diff1[info_m$item_code==i],min_order[2]),
                    seasonal = list(order=c(min_order[3],min_order[4],min_order[5]),period=4))
      predictv<-as.vector(predict(model2,n.ahead=2)$pred)
    }, error =function(e2){cat("ERROR :", conditionMessage(e2), "\n")})
    # 음수로 예측되는 것들을 0으로 바꾸어주는 장치를 만듦.
    # 에러메시지를 저장하고, 에러구분을 저장하는 객체를 만듦.
    tryCatch({
      if(info_m$logp[info_m$item_code==i]==0){
        predictv<-round(exp(predictv))
      } else {predictv<-round(predictv)}
      if(predictv[1]<0){
        predictv[1]<-0
      }
      if(predictv[2]<0){
        predictv[2]<-0
      }
      STAT_MSGE<-NULL
    }, error =function(e3){STAT_MSGE <<- conditionMessage(e3)})
    if(is.null(STAT_MSGE)){
      STAT_GUBN<-"S"
    } else{
      STAT_GUBN<-"E"
      predictv<-c(0,0)
      STAT_MSGE<-gsub("'","",STAT_MSGE)
    }
    # INSERT INTO로 쿼리를 보내서 저장함.
    tryCatch({
      dbSendQuery(con,paste0("INSERT INTO DM_MECHDMND_2 VALUES ",
                             paste0("(",paste(paste(WEEK_CUNT),
                                              paste0("'",i,"'"),
                                              paste0("'",2,"'"),
                                              0,
                                              predictv[1],
                                              0,
                                              predictv[2],
                                              paste0("'",DMND_YEAR,"'"),
                                              paste0("'",DMND_WEEK,"'"),
                                              paste0("'",CYCL_GUBN,"'"),
                                              paste0("'",STAT_GUBN,"'"),
                                              paste0("'",STAT_MSGE,"'"),
                                              "SYSDATE",
                                              paste0("'","hussu","'"),
                                              paste0("'","JOE","'"),
                                              sep = ","),")")))
    }, error = function(e4){f_item<<-append(f_item,i)})
    if(iter%%100==0){
      dbDisconnect(con)
      con <- dbConnect(drv, username = "MSFS_DM", password = "MSFSDM9", 
                       dbname = connect.string, prefetch = FALSE,
                       bulk_read = 1000L, stmt_cache = 0L, 
                       external_credentials = FALSE, sysdba = FALSE)
    }
  }
  db_commit(con)
  # 예측에 실패한 아이템들을 return함.
  return(f_item)
}
# fail_M에 예측에 실패한 아이템들을 저장하고, 다시한번 예측을 시도함.
# 그 이유는 S가 오고 그 다음 아이템이 E일 경우 쿼리가 들어가지 않는 현상이 있기 때문.
system.time(fail_M<-predict_M(mstime_month))
con <- dbConnect(drv, username = "MSFS_DM", password = "MSFSDM9", 
                 dbname = connect.string, prefetch = FALSE,
                 bulk_read = 1000L, stmt_cache = 0L, 
                 external_credentials = FALSE, sysdba = FALSE)

if(length(fail_M)>0){
  for (i in fail_M) {
    predict_M(mstime_month[i]) 
  }
}
#######################################################################################
# half data를 예측하는 predict_H이라는 함수를 만듦.
predict_H<-function(x){
  # forecast 패키지가 필요함.
  # 예측주차에 대한 정보를 할당함.
  # 순서쌍, 예측치, 누락 아이템, 반복횟수 등을 저장할 공간을 만듦.
  require(forecast)
  CYCL_GUBN<-2
  WEEK_CUNT<-week_info$WEEK_CUNT
  DMND_WEEK<-week_info$STND_WEEK
  DMND_YEAR<-week_info$STND_YEAR
  order1<-NULL
  fit1<-NULL
  predict1<-NULL
  item_code<-colnames(x)
  iter<-0
  f_item<-NULL
  for (i in item_code){
    # 반복횟수를 1씩 증가시키고 아이템코드와 반복횟수를 프린트함.
    # RMSE를 저장할 공간을 확보.
    iter<-iter+1
    print(i)
    print(iter)
    RMSE1<-rep(NA,324)
    for (i2 in 1:324) {
      # 각 순서쌍에 따라 324번 모형을 만들고 성능을 비교함.
      tryCatch({
        model = arima(x[i],order = c(com_[1,i2],info_h$diff1[info_h$item_code==i],com_[2,i2]),
                      seasonal = list(order=c(com_[3,i2],com_[4,i2],com_[5,i2]),period=6))
        fit1<-fitted.values(model)
        rmse<-sqrt(mean((x[,i]-fit1)^2,na.rm = T))
        RMSE1[i2]<-rmse
      }, error =function(e1){cat("ERROR :", conditionMessage(e1), "\n")})
    }
    # 위 324번 반복으로 최적의 순서쌍을 확보하고, 다시한번 모델을 구성하고 예측값을 구함.
    min_index = which.min(RMSE1)
    min_order = com_[,min_index]
    tryCatch({
      model2<-arima(x[i],order = c(min_order[1],info_h$diff1[info_h$item_code==i],min_order[2]),
                    seasonal = list(order=c(min_order[3],min_order[4],min_order[5]),period=6))
      predictv<-as.vector(predict(model2,n.ahead=2)$pred)
    }, error =function(e2){cat("ERROR :", conditionMessage(e2), "\n")})
    # 음수로 예측되는 것들을 0으로 바꾸어주는 장치를 만듦.
    # 에러메시지를 저장하고, 에러구분을 저장하는 객체를 만듦.
    tryCatch({
      if(info_h$logp[info_h$item_code==i]==0){
        predictv<-round(exp(predictv))
      } else {predictv<-round(predictv)}
      if(predictv[1]<0){
        predictv[1]<-0
      }
      if(predictv[2]<0){
        predictv[2]<-0
      }
      STAT_MSGE<-NULL
    }, error =function(e3){STAT_MSGE <<- conditionMessage(e3)})
    if(is.null(STAT_MSGE)){
      STAT_GUBN<-"S"
    } else{
      STAT_GUBN<-"E"
      predictv<-c(0,0)
      STAT_MSGE<-gsub("'","",STAT_MSGE)
    }
    # INSERT INTO로 쿼리를 보내서 저장함.
    tryCatch({
      dbSendQuery(con,paste0("INSERT INTO DM_MECHDMND_2 VALUES ",
                             paste0("(",paste(paste(WEEK_CUNT),
                                              paste0("'",i,"'"),
                                              paste0("'",2,"'"),
                                              0,
                                              predictv[1],
                                              0,
                                              predictv[2],
                                              paste0("'",DMND_YEAR,"'"),
                                              paste0("'",DMND_WEEK,"'"),
                                              paste0("'",CYCL_GUBN,"'"),
                                              paste0("'",STAT_GUBN,"'"),
                                              paste0("'",STAT_MSGE,"'"),
                                              "SYSDATE",
                                              paste0("'","hussu","'"),
                                              paste0("'","JOE","'"),
                                              sep = ","),")")))
    }, error = function(e4){f_item<<-append(f_item,i)})
    if(iter%%100==0){
      dbDisconnect(con)
      con <- dbConnect(drv, username = "MSFS_DM", password = "MSFSDM9", 
                       dbname = connect.string, prefetch = FALSE,
                       bulk_read = 1000L, stmt_cache = 0L, 
                       external_credentials = FALSE, sysdba = FALSE)
    }
  }
  db_commit(con)
  # 예측에 실패한 아이템들을 return함.
  return(f_item)
}
system.time(fail_H<-predict_H(mstime_half))
con <- dbConnect(drv, username = "MSFS_DM", password = "MSFSDM9", 
                 dbname = connect.string, prefetch = FALSE,
                 bulk_read = 1000L, stmt_cache = 0L, 
                 external_credentials = FALSE, sysdba = FALSE)
if(length(fail_H)>0){
  for (i in fail_H) {
    predict_H(mstime_half[i]) 
  }
}
# Week data를 예측하는 predict_W이라는 함수를 만듦.
predict_W<-function(x){
  # forecast 패키지가 필요함.
  # 예측주차에 대한 정보를 할당함.
  # 순서쌍, 예측치, 누락 아이템, 반복횟수 등을 저장할 공간을 만듦.
  require(forecast)
  CYCL_GUBN<-1
  WEEK_CUNT<-week_info$WEEK_CUNT
  DMND_WEEK<-week_info$STND_WEEK
  DMND_YEAR<-week_info$STND_YEAR
  order1<-NULL
  fit1<-NULL
  predict1<-NULL
  item_code<-colnames(x)
  iter<-0
  f_item<-NULL
  for (i in item_code){
    # 반복횟수를 1씩 증가시키고 아이템코드와 반복횟수를 프린트함.
    # RMSE를 저장할 공간을 확보.
    iter<-iter+1
    print(i)
    print(iter)
    RMSE1<-rep(NA,324)
    for (i2 in 1:324) {
      # 각 순서쌍에 따라 324번 모형을 만들고 성능을 비교함.
      tryCatch({
        model = arima(x[i],order = c(com_[1,i2],info_w$diff1[info_w$item_code==i],com_[2,i2]),
                      seasonal = list(order=c(com_[3,i2],com_[4,i2],com_[5,i2]),period=8))
        fit1<-fitted.values(model)
        rmse<-sqrt(mean((x[,i]-fit1)^2,na.rm = T))
        RMSE1[i2]<-rmse
      }, error =function(e1){cat("ERROR :", conditionMessage(e1), "\n")})
    }
    # 위 324번 반복으로 최적의 순서쌍을 확보하고, 다시한번 모델을 구성하고 예측값을 구함.
    min_index = which.min(RMSE1)
    min_order = com_[,min_index]
    tryCatch({
      model2<-arima(x[i],order = c(min_order[1],info_w$diff1[info_w$item_code==i],min_order[2]),
                    seasonal = list(order=c(min_order[3],min_order[4],min_order[5]),period=8))
      predictv<-as.vector(predict(model2,n.ahead=2)$pred)
    }, error =function(e2){cat("ERROR :", conditionMessage(e2), "\n")})
    # 음수로 예측되는 것들을 0으로 바꾸어주는 장치를 만듦.
    # 에러메시지를 저장하고, 에러구분을 저장하는 객체를 만듦.
    tryCatch({
      if(info_w$logp[info_w$item_code==i]==0){
        predictv<-round(exp(predictv))
      } else {predictv<-round(predictv)}
      if(predictv[1]<0){
        predictv[1]<-0
      }
      if(predictv[2]<0){
        predictv[2]<-0
      }
      STAT_MSGE<-NULL
    }, error =function(e3){STAT_MSGE <<- conditionMessage(e3)})
    if(is.null(STAT_MSGE)){
      STAT_GUBN<-"S"
    } else{
      STAT_GUBN<-"E"
      predictv<-c(0,0)
      STAT_MSGE<-gsub("'","",STAT_MSGE)
    }
    # INSERT INTO로 쿼리를 보내서 저장함.
    tryCatch({
      dbSendQuery(con,paste0("INSERT INTO DM_MECHDMND_2 VALUES ",
                             paste0("(",paste(paste(WEEK_CUNT),
                                              paste0("'",i,"'"),
                                              paste0("'",2,"'"),
                                              0,
                                              predictv[1],
                                              0,
                                              predictv[2],
                                              paste0("'",DMND_YEAR,"'"),
                                              paste0("'",DMND_WEEK,"'"),
                                              paste0("'",CYCL_GUBN,"'"),
                                              paste0("'",STAT_GUBN,"'"),
                                              paste0("'",STAT_MSGE,"'"),
                                              "SYSDATE",
                                              paste0("'","hussu","'"),
                                              paste0("'","JOE","'"),
                                              sep = ","),")")))
    }, error = function(e4){f_item<<-append(f_item,i)})
    if(iter%%100==0){
      dbDisconnect(con)
      con <- dbConnect(drv, username = "MSFS_DM", password = "MSFSDM9", 
                       dbname = connect.string, prefetch = FALSE,
                       bulk_read = 1000L, stmt_cache = 0L, 
                       external_credentials = FALSE, sysdba = FALSE)
    }
  }
  db_commit(con)
  # 예측에 실패한 아이템들을 return함.
  return(f_item)
}
system.time(fail_W<-predict_W(mstime_week))
con <- dbConnect(drv, username = "MSFS_DM", password = "MSFSDM9", 
                 dbname = connect.string, prefetch = FALSE,
                 bulk_read = 1000L, stmt_cache = 0L, 
                 external_credentials = FALSE, sysdba = FALSE)
if(length(fail_W)>0){
  for (i in fail_W) {
    predict_W(mstime_week[i]) 
  }
}



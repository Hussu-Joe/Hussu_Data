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
# drv: ����Ŭ ������ �̿��ϱ� ������ "Oracle" ���
# host: �����ּ�
# port: ��Ʈ
# sid: �����̸�
# connect.string�� �� ������ ������ string�� ����
# dbconnect�� ���ؼ� db�� ����
Sys.setenv(ORA_SDTZ = "KST")
Sys.setenv(TZ = "KST")
drv<-dbDriver("Oracle")

host <- "�ּ�"
port<-"��Ʈ"
sid<-"sid"
connect.string <- paste(
  "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
  "(CONNECT_DATA=(SID=", sid, ")))", sep = "")


con <- dbConnect(drv, username = "��������", password = "���", 
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
  # �� �Լ��� ���� DB�� ������Ʈ �Ǿ����� Ȯ���� �� �ִ�.
  # ���� ������ ������ �� �ִ� ��Ȳ�̸� Ready to work! ��� ǥ�ð� ���.
  created <- dbGetQuery(con,paste0("SELECT FNSH_DATE FROM DM_IF_WORKINFO WHERE WEEK_CUNT =",
                                (week_info$WEEK_CUNT-1)))
  
  if(length(created$FNSH_DATE)>0){
    print("Ready to work")
  }
}


workst()

##########################################################################################
#### 3. read TABLE
# dm_if_mechmas�� ��� �ֹ������� �����Ѵ�.
dm_if_mechmas <- dbGetQuery(con,"SELECT A.*
FROM DM_IF_MECHMAS_  A
WHERE A.INPU_GUBN = '1'")




##########################################################################################
#### 4. Create Time Serise Data ####
# dm_if_mechmas�� �ð迭 ������ ������ ���������� ��ȯ��Ű�� �Լ�
# to_timeseries�� �����.
to_timeseries <- function(x){
  # dplyr�� tidyr�� ����Ͽ� �Լ��� ����.
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
# mstime104�� to_timeseries�� dm_if_mechmas�� �μ��� ����־� �ð迭
# ������ �������� ����.
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
# 1�ֺ� ������ Itemcode�� ���ϴ� Classfyweeks��� �Լ��� ����.
classfyweeks <- function(x){
  x<-subset(x,x$sample_c>=24)
  weeks <- x$item_code[x$rmse_mean<0.7]
  weeks <- data.frame(weeks,stringsAsFactors = F)
  weeks$cycle <- rep("1",length(weeks))
  return(weeks)
}

# 2�ֺ� ������ Itemcode�� ���ϴ� Classfy2weeks��� �Լ��� ����.
classfy2weeks <- function(x){
  x<-subset(x,x$sample_c>=24)
  weeks2 <- x$item_code[x$rmse_mean>=0.7&x$rmse_mean<1.4]
  weeks2 <- data.frame(weeks2,stringsAsFactors = F)
  weeks2$cycle <- rep("2",length(weeks2))
  return(weeks2)
}

# 4�ֺ� ������ Itemcode�� ���ϴ� Classfy4weeks��� �Լ��� ����.

classfy4weeks<-function(x){
  x<-subset(x,x$sample_c>=24)
  weeks4<-x$item_code[x$rmse_mean>=1.4]
  weeks4<-data.frame(weeks4,stringsAsFactors = F)
  weeks4$cycle<-rep("3",length(weeks4))
  return(weeks4)
}

# 5���̻� 24�� �̸� ������
classfyimp1<-function(x){
  x<-subset(x,x$sample_c>=5)
  x<-subset(x,x$sample_c<24)
  imp1<-x$item_code
  imp1<-data.frame(imp1,stringsAsFactors = F)
  imp1$cycle<-rep("4",length(imp1))
  return(imp1)
}

# 5�̸ֹ� ������
classfyimp2<-function(x){
  x<-subset(x,x$sample_c<5)
  imp2<-x$item_code
  imp2<-data.frame(imp2,stringsAsFactors = F)
  imp2$cycle<-rep("5",length(imp2))
  return(imp2)
}

# weeks, wees_2, wees_4�� ���� 1�ֺ�, 2�ֺ�, 4�ֺ� itemcode�� �Ҵ���.
weeks <- classfyweeks(statweek)
weeks_2 <- classfy2weeks(statweek)
weeks_4 <- classfy4weeks(statweek)
imp1<-classfyimp1(statweek)
imp2<-classfyimp2(statweek)


##########################################################################################
#### 6. DM_IF_ITEMWORK Updating
# dbSendStatement�� DELETE ������ ���� DM_IF_ITEMWORK�� �����͸� ������ �غ� ��.
# upitem�̶�� �������������� ����.
# upitem�� WEEK_CUNT�� ITEM_CODE�� CYCLE_GUBN, JOBS_GUBN, JOBS_DATE�� �̷���� ����.
# WEEK_CUNT���� dm_if_mechmas�� WEEK_CUNT�� �ִ��� �־������� ���������� ����.
# CYCL_GUBN�� ITEM_CODE���� ITEM_CODE�� ���� ũ�� 3���� 1(week), 2(half), 3(month)�� ������ ����.
# �̷��� ������� Table�� DM_IF_ITEMWORK�� �������. db_commit���� commit�� ��.
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
# 1�ֺ� �����͸� mstime_week�� �Ҵ�
mstime_week <- mstime104[,weeks$weeks]

# 2�־� �����ִ� �Լ��� to_timeseriesH�� �Ҵ�.
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
# 2�ֺ� �����͸� mstime_half�� �Ҵ�.
mstime_half<-to_timeseriesH(mstime104[,weeks_2$weeks2])
# 4�־� �����ִ� �Լ��� to_timeseriesM�� �Ҵ�.
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
# 4�ֺ� �����͸� mstime_month�� �Ҵ��Ͽ���.
mstime_month<-to_timeseriesM(mstime104[,weeks_4$weeks4])

##########################################################################################
#### 8. Information dataframe ####
# �ð迭 ������ ���� �� ��Ȯ�ϰ� �ϱ� ���ؼ� ������ ����� ������, �α�ȭ�� �� �������� ����
# �Լ��� info�� �Ҵ��Ͽ���.
info<-function(x){
  #logp���� �α�ȭ�� �� ������ ���� �������� ���� ������ �������. 1�� �Ұ�, 0�� ������ �ǹ���.
  # unit1���� ù��° ���ַ�Ʈ�׽�Ʈ�� p.value�� �Ҵ��Ͽ���.
  # unit2���� �ι�° ���ַ�Ʈ�׽�Ʈ�� p.value�� �Ҵ��Ͽ���.
  # ���������� diff1���� ������ ����� �������� ���� ������ unit1, unit2�� ���Ͽ� ����Ͽ���.
  # item_code, logp, unit1, unit2, diff1���� �̷���� �Լ��� ��ȯ��.
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
# info_all, info_w, info_m, info_h�� ������ ����, �α�ȭ�� ���� ������ �Ҵ���.
info_all<-info(mstime104)
info_w<-info(mstime_week)
info_m<-info(mstime_month)
info_h<-info(mstime_half)
##########################################################################################
#### 9.Preparing Data ####
# ������ ����� �α�ȭ�� ���� ������ �̿��Ͽ� �ð迭�� �α�ȭ ��.
mstime_week[,info_w$item_code[info_w$logp==0]]<-log(mstime_week[,info_w$item_code[info_w$logp==0]])
mstime_half[,info_h$item_code[info_h$logp==0]]<-log(mstime_half[,info_h$item_code[info_h$logp==0]])
mstime_month[,info_m$item_code[info_m$logp==0]]<-log(mstime_month[,info_m$item_code[info_m$logp==0]])

# sARIMA ������ ã������ �������� �����ϱ� ���� �Լ��� combination�� �Ҵ�.
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
# com_�� sARIMA�� �������� �Ҵ��Ͽ���.
com_ <- combination()

#######################################################################################
#### 10. predict ####
# ������ ���� ��, Ŀ�ؼ��� �������� ��찡 ����ϰ� �߻��Ͽ� �ٽ��ѹ� DB�� ����.
con <- dbConnect(drv, username = "MSFS_DM", password = "MSFSDM9", 
                 dbname = connect.string, prefetch = FALSE,
                 bulk_read = 1000L, stmt_cache = 0L, 
                 external_credentials = FALSE, sysdba = FALSE)

#######################################################################################
# Monthly data�� �����ϴ� predict_M�̶�� �Լ��� ����.
predict_M<-function(x){
  # forecast ��Ű���� �ʿ���.
  # ���������� ���� ������ �Ҵ���.
  # ������, ����ġ, ���� ������, �ݺ�Ƚ�� ���� ������ ������ ����.
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
    # �ݺ�Ƚ���� 1�� ������Ű�� �������ڵ�� �ݺ�Ƚ���� ����Ʈ��.
    # RMSE�� ������ ������ Ȯ��.
    iter<-iter+1
    print(i)
    print(iter)
    RMSE1<-rep(NA,324)
    for (i2 in 1:324) {
      # �� �����ֿ� ���� 324�� ������ ����� ������ ����.
      tryCatch({
        model = arima(x[i],order = c(com_[1,i2],info_m$diff1[info_m$item_code==i],com_[2,i2]),
                      seasonal = list(order=c(com_[3,i2],com_[4,i2],com_[5,i2]),period=4))
        fit1<-fitted.values(model)
        rmse<-sqrt(mean((x[,i]-fit1)^2,na.rm = T))
        RMSE1[i2]<-rmse
      }, error =function(e1){cat("ERROR :", conditionMessage(e1), "\n")})
    }
    # �� 324�� �ݺ����� ������ �������� Ȯ���ϰ�, �ٽ��ѹ� ���� �����ϰ� �������� ����.
    min_index = which.min(RMSE1)
    min_order = com_[,min_index]
    tryCatch({
      model2<-arima(x[i],order = c(min_order[1],info_m$diff1[info_m$item_code==i],min_order[2]),
                    seasonal = list(order=c(min_order[3],min_order[4],min_order[5]),period=4))
      predictv<-as.vector(predict(model2,n.ahead=2)$pred)
    }, error =function(e2){cat("ERROR :", conditionMessage(e2), "\n")})
    # ������ �����Ǵ� �͵��� 0���� �ٲپ��ִ� ��ġ�� ����.
    # �����޽����� �����ϰ�, ���������� �����ϴ� ��ü�� ����.
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
    # INSERT INTO�� ������ ������ ������.
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
  # ������ ������ �����۵��� return��.
  return(f_item)
}
# fail_M�� ������ ������ �����۵��� �����ϰ�, �ٽ��ѹ� ������ �õ���.
# �� ������ S�� ���� �� ���� �������� E�� ��� ������ ���� �ʴ� ������ �ֱ� ����.
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
# half data�� �����ϴ� predict_H�̶�� �Լ��� ����.
predict_H<-function(x){
  # forecast ��Ű���� �ʿ���.
  # ���������� ���� ������ �Ҵ���.
  # ������, ����ġ, ���� ������, �ݺ�Ƚ�� ���� ������ ������ ����.
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
    # �ݺ�Ƚ���� 1�� ������Ű�� �������ڵ�� �ݺ�Ƚ���� ����Ʈ��.
    # RMSE�� ������ ������ Ȯ��.
    iter<-iter+1
    print(i)
    print(iter)
    RMSE1<-rep(NA,324)
    for (i2 in 1:324) {
      # �� �����ֿ� ���� 324�� ������ ����� ������ ����.
      tryCatch({
        model = arima(x[i],order = c(com_[1,i2],info_h$diff1[info_h$item_code==i],com_[2,i2]),
                      seasonal = list(order=c(com_[3,i2],com_[4,i2],com_[5,i2]),period=6))
        fit1<-fitted.values(model)
        rmse<-sqrt(mean((x[,i]-fit1)^2,na.rm = T))
        RMSE1[i2]<-rmse
      }, error =function(e1){cat("ERROR :", conditionMessage(e1), "\n")})
    }
    # �� 324�� �ݺ����� ������ �������� Ȯ���ϰ�, �ٽ��ѹ� ���� �����ϰ� �������� ����.
    min_index = which.min(RMSE1)
    min_order = com_[,min_index]
    tryCatch({
      model2<-arima(x[i],order = c(min_order[1],info_h$diff1[info_h$item_code==i],min_order[2]),
                    seasonal = list(order=c(min_order[3],min_order[4],min_order[5]),period=6))
      predictv<-as.vector(predict(model2,n.ahead=2)$pred)
    }, error =function(e2){cat("ERROR :", conditionMessage(e2), "\n")})
    # ������ �����Ǵ� �͵��� 0���� �ٲپ��ִ� ��ġ�� ����.
    # �����޽����� �����ϰ�, ���������� �����ϴ� ��ü�� ����.
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
    # INSERT INTO�� ������ ������ ������.
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
  # ������ ������ �����۵��� return��.
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
# Week data�� �����ϴ� predict_W�̶�� �Լ��� ����.
predict_W<-function(x){
  # forecast ��Ű���� �ʿ���.
  # ���������� ���� ������ �Ҵ���.
  # ������, ����ġ, ���� ������, �ݺ�Ƚ�� ���� ������ ������ ����.
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
    # �ݺ�Ƚ���� 1�� ������Ű�� �������ڵ�� �ݺ�Ƚ���� ����Ʈ��.
    # RMSE�� ������ ������ Ȯ��.
    iter<-iter+1
    print(i)
    print(iter)
    RMSE1<-rep(NA,324)
    for (i2 in 1:324) {
      # �� �����ֿ� ���� 324�� ������ ����� ������ ����.
      tryCatch({
        model = arima(x[i],order = c(com_[1,i2],info_w$diff1[info_w$item_code==i],com_[2,i2]),
                      seasonal = list(order=c(com_[3,i2],com_[4,i2],com_[5,i2]),period=8))
        fit1<-fitted.values(model)
        rmse<-sqrt(mean((x[,i]-fit1)^2,na.rm = T))
        RMSE1[i2]<-rmse
      }, error =function(e1){cat("ERROR :", conditionMessage(e1), "\n")})
    }
    # �� 324�� �ݺ����� ������ �������� Ȯ���ϰ�, �ٽ��ѹ� ���� �����ϰ� �������� ����.
    min_index = which.min(RMSE1)
    min_order = com_[,min_index]
    tryCatch({
      model2<-arima(x[i],order = c(min_order[1],info_w$diff1[info_w$item_code==i],min_order[2]),
                    seasonal = list(order=c(min_order[3],min_order[4],min_order[5]),period=8))
      predictv<-as.vector(predict(model2,n.ahead=2)$pred)
    }, error =function(e2){cat("ERROR :", conditionMessage(e2), "\n")})
    # ������ �����Ǵ� �͵��� 0���� �ٲپ��ִ� ��ġ�� ����.
    # �����޽����� �����ϰ�, ���������� �����ϴ� ��ü�� ����.
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
    # INSERT INTO�� ������ ������ ������.
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
  # ������ ������ �����۵��� return��.
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


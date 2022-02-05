timeseries_d<-function(x,x2){
  Date<-seq(as.Date(min(x$FROM_DATE),"%Y%m%d"),as.Date(max(x$FROM_DATE),"%Y%m%d")+6,by=1)
  ic<-x2$ITEM_CODE[x2$CYCL_GUBN=="4"| x2$CYCL_GUBN=="5"]
  u_date<-length(Date)
  u_ic<-length(ic)
  dd<-data.frame(matrix(data=rep(NA,(u_ic*u_date)),nrow=u_date))
  colnames(dd)<-ic
  for (i in ic) {
    qq<-subset(x,x$ITEM_CODE==i)
    qq<-group_by(qq,JUMU_DATE)
    qq<-summarise(qq,STCK_QNTY_=sum(STCK_QNTY))
    qq<-arrange(qq,JUMU_DATE)
    qq2<-data.frame(Date,matrix(rep(NA,u_date),ncol = 1))
    colnames(qq2)[1]<-"JUMU_DATE"
    qq2<-left_join(qq2,qq,by="JUMU_DATE")
    dd[,i]<-qq2$STCK_QNTY_
    p <- which(!is.na(dd[,i]))[1]
    if(is.na(p)){
      dd[,i]<-rep(0,u_date)
      next
    }
    print(p)
    dd[,i][p:u_date][is.na(dd[,i][p:u_date])]<-0
    p2 <- which(!dd[,i]==0)[1]
    if(is.na(p2)){
      next
    } else if(p2>1){
      dd[,i][1:(p2-1)] <- NA
    }
  }
  return(dd)
}
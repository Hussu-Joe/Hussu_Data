#080102mstime<-mstime[,intersect(colnames(mstime),master$상품코드)]
weather<-read.csv("c:/Users/hussu/Desktop/4주차/weatherfinal.csv",sep=",")
for (i in 1:7034){
  codebook$상품코드[i]<-paste0("X",codebook$상품코드[i])
}
#080104
sum(is.na(mstime$X106320))


for(i in 1:3448){
  p<-which(!is.na(mstime[,i]))[1]
  mstime[,i][p:104][is.na(mstime[,i][p:104])]<-0
}

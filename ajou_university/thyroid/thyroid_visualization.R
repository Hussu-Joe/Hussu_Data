library(readxl)
thy<-read_excel("c:/Users/hussu/Desktop/desc_thy/병리존재.xlsx",sheet = "sheet1",col_names = T)
# 2008년 수술환자를 제외하고 11337명의 환자를 팔로우업 하였음.
summary(thy$병리_나이_00)
sqrt(var(thy$병리_Size_00,na.rm=T))

thy$병리_성별_00<-as.factor(thy$병리_성별_00)
summary(thy$병리_LYMPH_C_IRRA_00)
thy$병리_MRND_00<-as.factor(thy$병리_MRND_00)
summary(thy$병리_MRND_00)
thy$병리_수술명_00<-as.factor(thy$병리_수술명_00)
summary(thy$병리_수술명_00)
summary(thy$병리_LYMPH_L_META_00)
summary(thy$병리_Size_00)
which(grepl("병리",colnames(thy))==T)
colnames(thy)[which(grepl("병리",colnames(thy))==T)]
byung<-thy[,which(grepl("병리",colnames(thy))==T)]
rai<-thy[,grepl("rai",colnames(thy))]
bun<-thy[,grepl("내분비",colnames(thy))]
synji<-thy[,grepl("신지로이드",colnames(thy))]
id<-thy$등록번호
ope_date<-thy$병리_수술일_00
byung<-data.frame(id,ope_date,byung)
rai<-data.frame(id,ope_date,rai)
synji<-data.frame(id,ope_date,synji)
bun<-data.frame(id,ope_date,bun)
sum(bun[1,grepl("시행일자",colnames(bun))]<=bun[1,"ope_date"],na.rm=T)
pre<-NULL
post<-NULL
for (i in 1:11337) {
  p<-sum(bun[i,grepl("시행일자",colnames(bun))]<=bun[i,"ope_date"],na.rm=T)
  if (p==0) {
    pre<-append(pre,0)
    post<-append(post,which(grepl("시행일자",colnames(bun)))[(p+1)])
  } else{
    pre<-append(pre,which(grepl("시행일자",colnames(bun)))[p])
    post<-append(post,which(grepl("시행일자",colnames(bun)))[(p+1)])
  }
}
post_ATPO<-NULL
post_Atg<-NULL
post_ft4<-NULL
post_t3<-NULL
post_tsh<-NULL
post_tg<-NULL


for (i in 1:11337) {
  post_ATPO<-append(post_ATPO,bun[i,(post[i]+1)])
  post_Atg<-append(post_Atg,bun[i,(post[i]+2)])
  post_ft4<-append(post_ft4,bun[i,(post[i]+3)])
  post_t3<-append(post_t3,bun[i,(post[i]+4)])
  post_tsh<-append(post_tsh,bun[i,(post[i]+5)])
  post_tg<-append(post_tg,bun[i,(post[i]+6)])
}

pre_ATPO<-NULL
pre_Atg<-NULL
pre_ft4<-NULL
pre_t3<-NULL
pre_tsh<-NULL
pre_tg<-NULL


for (i in 1:11337) {
  if (pre[i]==0) {
    next
  } else{
    pre_ATPO<-append(pre_ATPO,bun[i,(pre[i]+1)])
    pre_Atg<-append(pre_Atg,bun[i,(pre[i]+2)])
    pre_ft4<-append(pre_ft4,bun[i,(pre[i]+3)])
    pre_t3<-append(pre_t3,bun[i,(pre[i]+4)])
    pre_tsh<-append(pre_tsh,bun[i,(pre[i]+5)])
    pre_tg<-append(pre_tg,bun[i,(pre[i]+6)])
  }
}
ATPO<-bun[,grepl("Anti.TPO",colnames(bun))]
avg_ATPO<-NULL
for (i in 1:35) {
  a<-mean(ATPO[,i],na.rm = T)
  avg_ATPO<-append(avg_ATPO,a)
}
plot(1:35,avg_ATPO,"l",main = "AVG_ATPO")
ATG<-bun[,grepl("Anti.thyroglobulin",colnames(bun))]
avg_ATG<-NULL
for (i in 1:35) {
  a<-mean(ATG[,i],na.rm = T)
  avg_ATG<-append(avg_ATG,a)
}
plot(1:35,avg_ATG,"l",main = "AVG_ATG")

FT4<-bun[,grepl("Free.T4",colnames(bun))]
avg_ft4<-NULL
for (i in 1:35) {
  a<-mean(FT4[,i],na.rm = T)
  avg_ft4<-append(avg_ft4,a)
}
plot(1:35,avg_ft4,"l",main = "AVG_FT4")

T3<-bun[,grepl("T3",colnames(bun))]
avg_t3<-NULL
for (i in 1:34) {
  a<-mean(T3[,i],na.rm = T)
  avg_t3<-append(avg_t3,a)
}
plot(1:34,avg_t3,"l",main = "AVG_T3")

TSH<-bun[,grepl("TSH",colnames(bun))]
avg_tsh<-NULL
for (i in 1:35) {
  a<-mean(TSH[,i],na.rm = T)
  avg_tsh<-append(avg_tsh,a)
}
plot(1:35,avg_tsh,"l",main = "AVG_TSH")

TG<-bun[,grepl("Ag",colnames(bun))]
avg_tg<-NULL
for (i in 1:35) {
  a<-mean(TG[,i],na.rm = T)
  avg_tg<-append(avg_tg,a)
}
plot(1:35,avg_tg,"l",main = "AVG_TG")

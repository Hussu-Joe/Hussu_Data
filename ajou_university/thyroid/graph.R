library(readxl)
library(ggplot2)
library(forcats)
thy<-read_excel("c:/Users/hussu/Desktop/thy2/20190311_all.xlsx")
ggplot(thy,aes(x=fct_infreq(factor(병리_sex_00)),y=..count..))+geom_bar()+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+ggtitle("sex00")+
  ggsave("c:/Users/hussu/Desktop/thy2/sex00.png",dpi = 400)

colnames(thy)[22]

summary(thy$병리_age_00,na.rm=T)
sd(thy$병리_age_00,na.rm=T)
shapiro.test(thy$병리_age_00[sample(11875,5000,replace = F)])

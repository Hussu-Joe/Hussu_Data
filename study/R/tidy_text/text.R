library(dplyr)
library(tidytext)
library(readtext)
library(wordcloud)
text<-c("Because I could not stop for Death"
        ,"He kindly stopped for me"
        ,"The Carriage held but just Ourselves"
        ,"and Immortality")
text_df<-data_frame(line=1:4,text=text)
what <- text_df %>% unnest_tokens(word, text)
data(bibles)
data()
a<-readtext("c:/Users/hussu/Desktop/jeremiah/leviticus_16.txt",encoding = "utf-8")
?brewer.pal

pal <- brewer.pal(12,"Paired")
a$text<-gsub("\n","",a$text)
a$text<-gsub("    "," ",a$text)
b<-data_frame(line=1,text=a$text)
c <- b %>% unnest_tokens(word, text) %>% count(word, sort = T) %>% anti_join(stop_words)
c
c2<-c[1:63,]

d<-c2[-c(26, 30, 47, 49, 55),]


png("levi16.png",height = 960,width = 960,res=300)
d %>% with(wordcloud(word, n,colors = pal))
dev.off()
?wordcloud

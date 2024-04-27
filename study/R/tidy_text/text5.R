library(tidytext)
library(tm)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(methods)
library(Matrix)
library(janeaustenr)
library(tm.plugin.webmining)
library(purrr)

data("AssociatedPress",package = "topicmodels")

AssociatedPress
terms<-Terms(AssociatedPress)
ap_td <- tidy(AssociatedPress)
ap_td
ap_sentiments<- ap_td %>% inner_join(get_sentiments("bing"), by =c(term = "word"))
ap_sentiments

ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n>=200) %>%
  mutate(n = ifelse(sentiment=="negative", -n, n)) %>%
  mutate(term =  reorder(term, n)) %>%
  ggplot(aes(term, n, fill= sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentimet") +
  coord_flip()

ap_sentiments

data("data_corpus_inaugural",package = "quanteda")
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose=F)
inaug_dfm

inaug_td<- tidy(inaug_dfm)

inaug_td

inaug_td_idf<-inaug_td %>%
  bind_tf_idf(term, document,count) %>%
  arrange(desc(tf_idf))
inaug_td_idf

year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)",convert = T) %>%
  complete(year, term, fill = list(count=0))%>%
  group_by(year) %>%
  mutate(year_total = sum(count))


year_term_counts

year_term_counts %>% filter(term %in% c("god","america","foreign",
                                        "union","constitution","freedom")) %>%
  ggplot(aes(year, count/year_total))+
  geom_point() +
  geom_smooth() +
  facet_wrap(~term , scales = "free_y") +
  scale_y_continuous(labels= scales::percent_format()) +
  ylab("% frequency of word in inaugural address")


ap_td %>%
  cast_dtm(document, term, count)

ap_td %>%
  cast_dfm(document, term, count)

m <-ap_td %>%
  cast_sparse(document, term, count)

dim(m)

austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)

austen_dtm

data("acq")
acq_td<-tidy(acq)
acq_td
acq_tokens <- acq_td %>%
  select(-places) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

acq_tokens %>%
  count(word, sort = T)


acq_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))

company <- c("Microsoft", "Apple", "Google","Amazon", "Facebook",
             "Twitter","IBM", "Yahoo","Netflix")
symbol <- c("MSFT","AAPL","GOOG","AMZN","FB","TWTR","IBM","YHOO","NFLX")


download_article <- function(symbol){
  
  WebCorpus(YahooFinanceSource(paste0(symbol)))
}
YahooFinanceSource("MSFT")


?YahooFinanceSource
stock_articles <-data_frame(company=company, symbol=symbol) %>%
  mutate(corpus=map(symbol,download_article))

map(symbol,download_article)

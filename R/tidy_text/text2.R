library(janeaustenr)
library(gutenbergr)

library(tidyr)
library(stringr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(reshape2)
data(stop_words)
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = T)))) %>%
  ungroup()
original_books

tidy_books <- original_books %>% 
  unnest_tokens(word, text)

tidy_books <- tidy_books %>%
  anti_join(stop_words)
tidy_books
tidy_books %>% count(word, sort = T) %>%
  filter(n >600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()


tidy_books %>% count(word, sort = T) %>%
  filter(n >600) %>%
  mutate(word = reorder(word, n))

####################################################
hgwells <- gutenberg_download(c(35,36,5230,159))

tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_hgwells %>%
  count(word, sort = T)

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte %>%
  count(word, sort = T)

tidy_bronte

frequency <- bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),
                       mutate(tidy_hgwells, author = "H.G wells"),
                       mutate(tidy_books, author = "Jane Austen")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, 'Bronte Sisters':'H.G wells')



######## chap 2 #######
library(tidytext)
?get_sentiments
sentiments
get_sentiments("afinn")

####################
library(janeaustenr)
library(dplyr)
library(stringr)
library(textdata)
tidy_books<- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = T)))) %>% 
  ungroup() %>%
  unnest_tokens(word, text)

lexicon<-lexicon_nrc()

tidy_books

nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment =="joy")

get_sentiments("afinn")

tidy_books %>%
  filter(book =="Emma") %>%
  inner_join(nrcjoy) %>%
  count(word, sort =T)

##########################
library(tidyr)
janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n , fill = 0) %>%
  mutate(sentiment = positive - negative)
janeaustensentiment

library(ggplot2)
ggplot(janeaustensentiment, aes(index, sentiment, fill=book)) +
  geom_col(show.legend = F)+
  facet_wrap(~book, ncol =2, scales = "free_x")

pride_prejudice<- tidy_books %>%
  filter(book == "Pride & Prejudice")


afinn <- pride_prejudice %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method ="AFINN")

bing_and_nrc <-bind_rows(
  pride_prejudice %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive","negative"))) %>%
    
    mutate(method="NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment =positive - negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = F) +
  facet_wrap(~method, ncol=1, scales = "free_y")

######## nrc에는 몇개의 부정단어?
get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive","negative")) %>%
  
  count(sentiment)

get_sentiments("bing") %>%
  filter(sentiment %in% c("positive","negative")) %>%
  
  count(sentiment)

bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort= T) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = F) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x=NULL) +
  coord_flip()


tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word,n, max.words = 100))

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = T) %>%
  acast(word~sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20","gray80"),
                   max.words = 100)

PandP_sentences<-data_frame(text=prideprejudice) %>%
  unnest_tokens(sentence, text, token="sentences")

austen_chapters<-austen_books()  %>%
  group_by(book) %>%
  unnest_tokens(chapter,text, token = "regex",
                pattern="Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

austen_chapters %>%
  group_by(book) %>%
  summarise(chapter = n())

bingnegative <- get_sentiments("bing") %>%
  filter(sentiment=="negative")

wordcounts<- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())

tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  top_n(1) %>%
  ungroup()

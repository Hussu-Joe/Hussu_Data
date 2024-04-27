library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
data("AssociatedPress")
AssociatedPress
ap_lda <- LDA(AssociatedPress, k=2, control = list(seed=1234))
ap_topics <- tidy(ap_lda,matrix = "beta")
ap_topics

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = F)+
  facet_wrap(~topic, scales = "free") +
  coord_flip()

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic",topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

ap_documents <-tidy(ap_lda, matrix = "gamma")
ap_documents

tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))

titles<-c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
          "Pride and Prejudice", "Great Expectations")

library(gutenbergr)

books<- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

books

library(stringr)

#각기 1개 장을 대표하는 문서들로 나눈다.
reg<- regex("^chapter ", ignore_case = T)
by_chapter <-books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, reg))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)
by_chapter


# 단어들로 분리한다.
by_chapter_word <-by_chapter %>%
  unnest_tokens(word, text)

by_chapter_word

#문서 - 단어 카운트
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = T) %>%
  ungroup()

word_counts

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm

chapters_lda<-LDA(chapters_dtm, k=4, control = list(seed=1234))

chapters_lda

chapter_topics <-tidy(chapters_lda, matrix = "beta")

chapter_topics

top_terms <-chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

library(ggplot2)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = F)+
  facet_wrap(~ topic, scales = "free")+
  coord_flip()

chapters_gamma<-tidy(chapters_lda, matrix = "gamma")

chapters_gamma<-chapters_gamma %>% separate(document, c("title","chapter"), sep="_",convert = T)
chapters_gamma

chapters_gamma %>%
  mutate(title = reorder(title, gamma*topic)) %>%
  ggplot(aes(factor(topic),gamma)) +
  geom_boxplot()+
  facet_wrap(~title)

chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

chapter_classifications

book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

book_topics
chapter_classifications %>% inner_join(book_topics, by="topic") %>%
  filter(title != consensus)

assignments <- augment(chapters_lda, data = chapters_dtm)

assignments <-assignments %>%
  separate(document,c("title","chapter"), sep = "_", convert = T) %>%
  inner_join(book_topics, by=c(".topic"="topic"))



assignments %>%
  count(title, consensus, wt =count) %>%
  group_by(title) %>%
  mutate(percent = n /sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = percent_format())+
  theme_minimal() +
  theme(axis.text.x =  element_text(angle = 90, hjust =1),
        panel.grid = element_blank()) +
  labs(x= "Book words were assigned to",
       y= "Book words came from",
       fill ="% of assignments")

wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))

word_counts %>%
  filter(word == "flopson")

#mallet
library(mallet)

collapsed<- by_chapter_word %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_replace(word,"'","")) %>%
  group_by(document) %>%
  summarize(text = paste(word, collapse = " "))

# 불용어를 넣을 빈 파일을 생성
file.create(empty_file <-tempfile())

docs<- mallet.import(collapsed$document, collapsed$text, empty_file)

mallet_model <- MalletLDA(num.topics = 4)

mallet_model$loadDocuments(docs)
mallet_model$train(100)

tidy(mallet_model)

tidy(mallet_model, matrix = "gamma")

term_counts <-rename(word_counts, term = word)

augment(mallet_model, term_counts)

library(dplyr)
library(tidytext)
library(janeaustenr)
library(ggplot2)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)
austen_bigrams
austen_bigrams %>%
  count(bigram, sort = T)

library(tidyr)

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c('word1','word2'),sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = T)

bigram_counts

bigram_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep =" ")

bigram_united

austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>%
  separate(trigram, c("word1","word2","word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = T)

# bigram 분석
bigrams_filtered %>%
  filter(word2=="street") %>%
  count(book, word1, sort = T)

# 각 도서에 나오는 바이그램의 tf-idf 시각화
bigram_tf_idf<- bigram_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf

bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = T)

AFINN<-get_sentiments("afinn")

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = T) %>%
  ungroup()

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n*value, fill= n*value > 0)) +
  geom_col(show.legend = F) + 
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences")+
  coord_flip()

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by=c(word2="word")) %>%
  count(word1, word2, value, sort = T) %>%
  ungroup()

# 시각화
library(igraph)

bigram_counts

bigram_graph <- bigram_counts %>%
  filter(n>20) %>%
  graph_from_data_frame()

bigram_graph

library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name), vjust = 1, hjust =1)

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha =n), show.legend = F,
                 arrow = a, end_cap= circle(.07,"inches")) +
  geom_node_point(color = "lightblue", size= 5) +
  geom_node_text(aes(label=name), vjust = 1, hjust = 1) +
  theme_void()

austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section >0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

library(widyr)

word_pairs<- austen_section_words %>%
  pairwise_count(word, section, sort = T)

word_pairs %>%
  filter(item1 =="darcy")

word_cors <-austen_section_words %>%
  group_by(word) %>%
  filter(n() >=20) %>%
  pairwise_cor(word, section, sort=T)

word_cors

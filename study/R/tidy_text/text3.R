library(dplyr)
library(janeaustenr)
library(tidytext)
library(ggplot2)
library(gutenbergr)

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = T) %>%
  ungroup()

book_words

total_words <-book_words %>%
  group_by(book) %>%
  summarize(total = sum(n))


book_words<- left_join(book_words, total_words,"book")

book_words

ggplot(book_words, aes(n/total, fill=book)) +
  geom_histogram(show.legend = F) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")


## 지프
freq_by_rank <- book_words %>%
  group_by(book) %>%
  mutate(rank = row_number(),
         'term frequency'= n/total)


freq_by_rank %>%
  ggplot(aes(rank, 'term frequency', color = book))+
  geom_line(size= 1.1, alpha = 0.8, show.legend = F)+
  scale_x_log10()+
  scale_y_log10()


book_words <- book_words %>%
  bind_tf_idf(word, book, n)
book_words


rank_subset <- freq_by_rank %>%
  filter(rank <500, rank >10)
lm(log10(rank_subset$`term frequency`) ~ log10(rank_subset$rank))
sum(is.infinite(log10(rank_subset$`term frequency`)))

book_words <- book_words %>%
  bind_tf_idf(word, book, n)

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

book_words %>% arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(book) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word,tf_idf, fill = book))+
  geom_col(show.legend = F)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~book, ncol =2, scales = "free")+
  coord_flip()

# 물리학
physics <- gutenberg_download(c(37729, 14725, 13476, 5001),
                              meta_fields = "author")

physics_words <- physics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = T) %>%
  ungroup()

plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))
physics_words

plot_physics %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf,, fill=author)) +
  geom_col(show.legend = F) +
  labs(x=NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales ="free")+
  coord_flip()

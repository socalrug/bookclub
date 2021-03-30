# John Peach
library(tidyverse)
library(tidytext)
library(janeaustenr)

austen_books() %>%
  group_by(book) %>%
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>%
  drop_na(bigram) %>%
  separate(bigram, paste0('word', 1:2)) %>%
  dplyr::filter(!word1 %in% stop_words$word) %>%
  dplyr::filter(!word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = ' ') %>%
  count(bigram, sort = TRUE) %>%
  bind_tf_idf(bigram, book, n) %>%
  slice_head(n, n = 5) %>%
  ggplot(aes(y = fct_reorder(bigram, n), x = n, colour = book)) +
    facet_wrap(~book, ncol = 2, scales = 'free') +
    geom_bar(stat = 'identity', aes(fill = book)) +
    theme(legend.position = 'none') +
    labs(y = NULL)


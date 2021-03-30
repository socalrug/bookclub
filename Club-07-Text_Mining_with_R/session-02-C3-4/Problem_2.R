# John Peach
library(tidyverse)
library(tidytext)
library(janeaustenr)

austen_books() %>%
  dplyr::filter(book == 'Sense & Sensibility') %>%
  select(text) %>%
  unnest_tokens(trigram, text, token = 'ngrams', n = 3) %>%
  drop_na(trigram) %>%
  separate(trigram, paste0('word', 1:3)) %>%
  dplyr::filter(!word1 %in% stop_words$word) %>%
  dplyr::filter(!word2 %in% stop_words$word) %>%
  dplyr::filter(!word3 %in% stop_words$word) %>%
  unite(trigram, word1, word2, word3, sep = ' ') %>%
  count(trigram) %>%
  dplyr::filter(n > 1) %>%
  arrange(n) %>%
  ggplot(aes(x = n, y = fct_reorder(trigram, n))) +
    geom_bar(stat = 'identity') +
    labs(y = NULL)

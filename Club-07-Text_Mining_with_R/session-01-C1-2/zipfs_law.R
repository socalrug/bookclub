library(tidytext)
library(janeaustenr)

austen_books() %>%
  select(text) %>%
  unnest_tokens(word, text) %>%
  count(word) %>%
  mutate(frequency = n / sum(n)) %>%
  arrange(-n) %>%
  mutate(rank = row_number()) %>%
  ggplot(aes(x = rank, y = frequency)) +
    geom_point() +
    scale_x_log10() +
    scale_y_log10() +
    theme_minimal()

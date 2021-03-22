library(tidytext)
library(gutenbergr)


df <- gutenberg_download(1661)
df <- austen_books()
df %>%
  select(text) %>%
  dplyr::filter(length(text) > 0) %>%
  unnest_tokens(characters, text, token = 'characters') %>%
  count(characters) %>%
  mutate(frequency = n / sum(n)) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(characters, frequency), y = frequency)) +
    geom_col() +
    coord_flip() +
    labs(x = 'Frequency', y = 'Character')

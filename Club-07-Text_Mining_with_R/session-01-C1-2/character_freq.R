library(tidytext)
library(janeaustenr)

austen_books() %>%
  select(text) %>%
  unnest_tokens(characters, text, token = 'characters') %>%
  count(characters) %>%
  mutate(frequency = n / sum(n)) %>%
  ggplot(aes(x = reorder(characters, frequency), y = frequency)) +
    geom_col() +
    coord_flip() +
    labs(x = 'Frequency', y = 'Character')

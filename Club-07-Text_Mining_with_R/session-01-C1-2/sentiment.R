library(tidytext)
library(janeaustenr)

austen_books() %>%
  dplyr::filter(book == 'Sense & Sensibility') %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case=TRUE)))) %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments('afinn')) %>%
  group_by(chapter) %>%
  select(chapter, value) %>%
  dplyr::summarise(sentiment = mean(value)) %>%
  ggplot(aes(x = chapter, y = sentiment)) +
    geom_line() +
    theme_minimal() +
    labs(x = "Chapter", y = "Sentiment", title = "Normalized Sentiment by Chapter")

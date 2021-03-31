##########################
##EXERCISES CHAPTERS 3-4##
##########################
##Author: Judith Borghouts
##3-29-2021

##1. Find the top 5 highest tf-idf bigrams per book.

#Get bigrams, to see how often word X is followed by word Y.
#Count occurrence of bigrams and sort in descending order.
austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)
austen_bigrams

#Separate words in bigram to have two columns.
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
#Filter out bigrams where the first or second word is a stop word.
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
#New bigram counts
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
bigram_counts
#Unite separated words into one bigram again.
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
#Get tf-idf of bigrams and sort in descending order.
bigram_tf_idf<- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

#Visualize the top 5 highest tf-idf bigrams, grouped by book.
bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>%
  group_by(book) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol=2, scales = "free") +
  coord_flip()

##2. Find trigrams that occur more than once in the novel Sense and Sensibility.
#Filter on book Sense and Sensibility.
sense_sensibility <- austen_books() %>% 
  filter(book == "Sense & Sensibility")

#Get trigrams.
sense_trigrams <- sense_sensibility %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

#Count occurrence of trigrams and sort in descending order.
sense_trigrams %>%
  count(trigram, sort = TRUE)

#Separate the words of the trigram and filter out trigrams containing stop words.
trigrams_separated <- sense_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)
#New trigram counts; filter to show trigrams that occurred more than once.
trigram_counts <- trigrams_filtered %>%
  count(word1, word2, word3, sort = TRUE) %>%
  filter(n > 1) 
#Unite separated words back into trigram and visualize as bar graph.
trigram_united <- trigram_counts %>%
  unite(trigram, word1, word2, word3, sep = " ") %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(x = n, y = trigram)) +
  geom_col() +
  labs(y = NULL)

trigram_united

##3. Visualize words in Pride and Prejudice that have at least a 0.1 correlation with 'darcy'.
#Filter on book Pride and Prejudice.
austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)
austen_section_words

#Visualize words that have at least a 0.1 correlation with darcy.
word_cors <- austen_section_words %>%
  group_by(word) %>%
  pairwise_cor(word, section, sort = TRUE) %>%
  ungroup() %>%
  filter(item1 == "darcy") %>%
  filter(correlation > .1) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
word_cors
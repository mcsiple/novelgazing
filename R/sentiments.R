#Sentiments, etc
read_books_data <- goodreads

clean_book_descs <- read_books_data %>%
  mutate(clean_desc = str_replace_all(book_description, "[^a-zA-Z\\s]", " ")) %>%
  mutate(clean_desc = str_trim(clean_desc, side = "both")) %>%
  select(-book_genres)

library(tidytext)

descs_unnested <- clean_book_descs %>%
  unnest_tokens(word, clean_desc) %>%
  select(-book_description)

data('stop_words')

tidy_books <- descs_unnested %>%
  anti_join(stop_words, by = "word")

tidy_book_words <- tidy_books %>%
  count(word, sort = TRUE)
tidy_book_words

data(sentiments)

word_sentiments <- tidy_book_words %>%
  inner_join(get_sentiments("afinn"), by = "word")

word_sentiments
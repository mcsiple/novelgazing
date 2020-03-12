#Sentiments, etc
library(tidyverse)
library(tidytext)

goodreads <- read.csv(here::here('output','goodreads_read.csv'))
read_books_data <- goodreads

clean_book_descs <- read_books_data %>%
  mutate(clean_desc = str_replace_all(book_description, "[^a-zA-Z\\s]", " ")) %>%
  mutate(clean_desc = str_trim(clean_desc, side = "both"))


descs_unnested <- clean_book_descs %>%
  unnest_tokens(word, clean_desc) %>%
  select(-book_description)

data('stop_words')

tidy_books <- descs_unnested %>%
  anti_join(stop_words, by = "word")

nbooks <- tidy_books %>%
  group_by(year_read) %>%
  summarize(nbooks = length(unique(title))) 

word_bing <- tidy_books %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(year_read,sentiment) %>%
  summarize(n = length(sentiment)) %>%
  left_join(nbooks,by = 'year_read') %>%
  mutate(n_scaled = (n/nbooks)*ifelse(sentiment == 'negative',-1,1)) #negative or pos words scaled to the number of descriptions

word_bing %>%
  group_by(year_read) %>%
  summarize(score = mean(n_scaled)) %>%
  ggplot(aes(year_read,score)) +geom_line()

# NOTE: maybe turn this into a stacked density plot! with positive and negative sentiment scores
word_bing %>%
  ggplot(aes(year_read,n_scaled,fill = sentiment)) +
  geom_col(lwd=1) +
  scale_fill_manual('Sentiment',
                    values = calecopal::cal_palette('chaparral1')) + 
  xlab('Year') +
  ylab('Word positivity') +
  ggpomological::theme_pomological()

tidy_book_words <- tidy_books %>%
  group_by(year_read,word) %>%
  count(word, sort = TRUE)
tidy_book_words

# NOTE: want to show this as a cute kable table
data(sentiments)
word_sentiments <- tidy_book_words %>%
  inner_join(get_sentiments("afinn"), by = "word") 


word_sentiments

word_sentiments %>% 
  filter(!is.na(year_read)) %>%
  ggplot(aes(value)) + 
  geom_bar() +  
  facet_wrap(~year_read) + # NOTE: change this to little dots! the new density thing
  ggpomological::theme_pomological(base_size = 14) +
  xlab('Score') +
  ylab('Count') +
  labs(title = 'Sentiment scores',
       subtitle = 'AFINN sentiment score frequency by year',
       caption = 'Data: Goodreads')



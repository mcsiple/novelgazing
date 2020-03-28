#Sentiments, etc
#library(tidyverse)
#library(tidytext)

# bookpal <- c('#ff7506', #dark orange
#              '#00eaff', #(cyan)
#              '#ffdf06', #(dark yellow)
#              '#ffee7d', #(light yellow)
#              '#ff2673', #(magenta)
#              '#7df4ff') #(light cyan)

#goodreads <- read.csv(here::here('output','goodreads_read.csv'))
#read_books_data <- goodreads

 #afinn_man <- get_sentiments('afinn')
 #save(afinn_man,file = 'AFINNdat.RData') # sorry!

tidy_the_books <- function(read_books_data){
  #read_books_data is the scraped data 'goodreads_read'
  clean_book_descs <- read_books_data %>%
    mutate(clean_desc = str_replace_all(book_description, "[^a-zA-Z\\s]", " ")) %>%
    mutate(clean_desc = str_trim(clean_desc, side = "both"))
  
  descs_unnested <- clean_book_descs %>%
    unnest_tokens(word, clean_desc) %>%
    select(-book_description)
  
  data('stop_words')
  #print(head(stop_words))
  tidied_books <- descs_unnested %>%
    anti_join(stop_words, by = "word")
  return(tidied_books)
}

#tidied = tidy_the_books(read_books_data)

# getnbooks <- function(tidied_books){
#   nbooks <- tidied_books %>%
#     group_by(year_read) %>%
#     summarize(nbooks = length(unique(title))) 
#   return(nbooks)
# }

get_bing_plot <- function(tidied_books,pal){
  nbooks <- tidied_books %>%
    group_by(year_read) %>%
    summarize(nbooks = length(unique(title))) 
  word_bing <- tidied_books %>%
    inner_join(get_sentiments("bing"), by = "word") %>%
    group_by(year_read,sentiment) %>%
    summarize(n = length(sentiment)) %>%
    left_join(nbooks,by = 'year_read') %>%
    mutate(n_scaled = (n/nbooks)*ifelse(sentiment == 'negative',-1,1)) # negative or pos words scaled to the number of descriptions
  
  # NOTE: maybe turn this into a stacked density plot! with positive and negative sentiment scores
  bp <- word_bing %>%
    ggplot(aes(year_read,n_scaled,fill = sentiment)) +
    geom_hline(yintercept = 0,lty=2,colour='darkgrey')+
    geom_col(lwd=0.6,colour = 'darkgrey') +
    scale_fill_manual('',
                      values = pal[c(5,3)]) + 
    xlab('Year') +
    ylab('Word positivity') +
    #labs(title ='Bing sentiment score'),
    ggsidekick::theme_sleek(base_size = 14) +
    theme(legend.position = 'bottom')
  return(bp)
}

get_word_table <- function(tidied_books){
  tidy_book_words <- tidied_books %>%
    group_by(year_read,word) %>%
    count(word, sort = TRUE)
  
  # most frequent words in descriptions of your books
  return(tidy_book_words) # NOTE: show this as a cute kable table
}

get_AFINN_plot <- function(tidied_books,afinnsentiments){
  tidy_book_words <- get_word_table(tidied_books)
  word_sentiments <- tidy_book_words %>%
    inner_join(afinnsentiments, by = "word") 
  #print(word_sentiments)
  
  afinnplot <- word_sentiments %>% 
    filter(!is.na(year_read)) %>%
    ggplot(aes(value)) + 
    geom_bar() +  
    facet_wrap(~year_read) + # NOTE: change this to little dots! the new density thing
    ggsidekick::theme_sleek(base_size=12) +
    xlab('Score') +
    ylab('Count') +
    labs(title = 'Sentiment scores',
         subtitle = 'AFINN sentiment score frequency by year',
         caption = 'Data: Goodreads')
  return(afinnplot)
}

#get_AFINN_plot(tidied_books = tidied)

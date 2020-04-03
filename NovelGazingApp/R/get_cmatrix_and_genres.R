# get big fun matrix for further analysis
get_cmatrix_and_genres <- function(fulldata){
  #fulldata includes genres and everything, almost exact same formula as for scraped data
  goodreads <- fulldata
  
  goodreads_read <- 
    goodreads %>%
    filter(exclusive_shelf =='read') %>%
    select(-book_genres)
  
  
  # save genre data for read books
  genres <- goodreads %>%
    filter(exclusive_shelf =='read') %>%
    select(book_genres,year_read) %>%
    mutate(id = 1:n()) %>%
    unnest_longer(book_genres) %>%
    filter(!is.na(book_genres)) %>%
    filter(!is.na(year_read)) %>%
    filter(textcat::textcat(book_genres)=='english') %>%
    mutate(book_genres = sub("\\ /.*", "", book_genres)) %>%
    mutate(book_genres = sub("\\ --.*", "", book_genres)) %>%
    mutate(book_genres = stringr::str_to_sentence(book_genres)) %>%
    mutate(book_genres = trimws(book_genres))
  
  # save genres by month
  genres_month <- goodreads %>%
    filter(exclusive_shelf =='read') %>%
    select(book_genres,month_read) %>%
    mutate(id = 1:n()) %>%
    unnest_longer(book_genres) %>%
    filter(!is.na(book_genres)) %>%
    filter(!is.na(month_read)) %>%
    filter(textcat::textcat(book_genres)=='english') %>%
    mutate(book_genres = sub("\\ /.*", "", book_genres)) %>%
    mutate(book_genres = sub("\\ --.*", "", book_genres)) %>%
    mutate(book_genres = stringr::str_to_sentence(book_genres)) %>%
    mutate(book_genres = trimws(book_genres))
  
  # Turn genres by year into 'community matrix'
  community <-  genres %>% 
    pivot_longer(c(-id,-year_read)) %>% 
    dplyr::count(year_read, value) %>% # to give each book its own diversity, change to dplyr::count(id, value)
    pivot_wider(
      names_from = value, 
      values_from = n, 
      values_fill = list(n = 0)
    ) %>%
    as.data.frame()
  
  community_toread <- goodreads %>%
    select(book_genres,year_read) %>%
    mutate(id = 1:n()) %>%
    unnest_longer(book_genres) %>%
    pivot_longer(c(-id,-year_read)) %>% 
    dplyr::count(year_read, value) %>% 
    pivot_wider(
      names_from = value, 
      values_from = n, 
      values_fill = list(n = 0)
    ) %>%
    as.data.frame()
  
  
  return(list(community = community,
              community_toread = community_toread,
              genres = genres,
              genres_month = genres_month,
              goodreads_read = goodreads_read))
}
#get_cmatrix_and_genres(fulldata = full_data)

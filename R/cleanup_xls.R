library(tidyverse)
library(snakecase) #cool Mara thing
library(lubridate)

cleanup_xls <- function(my_books){
  # Cleanup names
  orig_colnames <- colnames(my_books)  # get original colnames
  new_colnames <- to_any_case(orig_colnames, case = "snake")
  colnames(my_books) <- new_colnames  # assign new colnames
  #colnames(my_books)  # check to make sure it worked

  keeper_cols <- c("book_id","title","author","isbn","my_rating","average_rating","date_read","date_added")
  
  books_myvars <- my_books %>%
    select(one_of(keeper_cols))
  read_books <- books_myvars %>%
    filter(!is.na(date_read))
  
  #Cleanup dates
  read_books <- read_books %>%
    mutate(date_read = ymd(date_read)) %>%
    mutate(year_read = year(date_read)) %>%
    arrange(desc(date_read))
  
  return(read_books)
}

# Test
# filename = here::here('data','goodreads_library_export.csv')
# my_books <- read.csv(filename)
# cleanup_data(my_books)
library(lubridate)
library(here)
library(tidyverse)
library(ggpomological)
library(reshape2)
library(snakecase)

#testfile <- here::here('data','goodreads_library_export.csv')

cleanup_csv <- function(csvfile){
  my_books <- read.csv(csvfile,header = TRUE)
  orig_colnames <- colnames(my_books)  # get original colnames
  new_colnames <- to_any_case(orig_colnames, case = "snake")
  colnames(my_books) <- new_colnames  # assign new colnames
  colnames(my_books)  # check to make sure it worked
  keeper_cols <- c("book_id", "title", "author", "my_rating", "number_of_pages", 
                   "original_publication_year", "date_read", "date_added", "bookshelves",
                   "my_rating","average_rating")
  read_books <- my_books %>%
    select(one_of(keeper_cols)) %>%
    mutate(date_read = ymd(date_read),
           year_read = year(date_read))
  return(read_books)
}

#cleanup_csv(testfile)
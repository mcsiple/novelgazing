
cleanup_csv <- function(my_books){
  # my_books is a dataframe from read.csv()
  orig_colnames <- colnames(my_books)  # get original colnames
  new_colnames <- to_any_case(orig_colnames, case = "snake")
  colnames(my_books) <- new_colnames  # assign new colnames
  colnames(my_books)  # check to make sure it worked
  keeper_cols <- c("book_id", "title", "author", "isbn", "my_rating", "number_of_pages", 
                   "original_publication_year", "date_read", "date_added", "bookshelves",
                   "exclusive_shelf","my_rating","average_rating")
  
  read_books <- my_books %>%
    select(one_of(keeper_cols)) %>%
    mutate(isbn = str_remove_all(isbn, '[^[:alnum:]]')) %>%
    mutate(isbn = as.character(isbn)) %>%
    mutate(date_read = ymd(date_read),
           year_read = year(date_read),
           month_read = month(date_read),
           date_added = ymd(date_added),
           year_added = year(date_added))
  return(read_books)
}

#cleanup_csv(my_books)

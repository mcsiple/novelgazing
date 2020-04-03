# Use curl to get book data  -----------------------------------------
# This is subpar but c'est la vie
library(curl)
library(httr)

get_genre <- function(cleaned_books,i){
  isbn = cleaned_books$isbn[i]
  if(isbn =='') return (NA) else{
    r <- httr::GET(paste('https://openlibrary.org/api/books?bibkeys=ISBN:',isbn,'&jscmd=data&format=json',sep=''))
    rl <- httr::content(r, "parsed")
    if(length(rl)==0){return(NA)}else{
      subjlist <- rl[[1]]$subjects
      genres <- unlist(map(subjlist,'name'))
      return(genres)
    }}
}

# cleaned_books <- cleaned_csv1
# genrelist <- list()
# s <- Sys.time()
# for(i in 1:nrow(cleaned_books)){
#   genrelist[[i]] <- get_genre(cleaned_books = cleaned_books,i = i)
#   print(i)
#   print(genrelist[i])
# }
# st <- Sys.time()
# print(st - s)
# full_data <- tibble(title = cleaned_books$title,
#                     author = cleaned_books$author,
#                     month_read = cleaned_books$month_read,
#                     year_read = cleaned_books$year_read,
#                     exclusive_shelf = cleaned_books$exclusive_shelf,
#                     book_genres = genrelist)

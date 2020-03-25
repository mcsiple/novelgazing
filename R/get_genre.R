# Use curl to get book data MAYBE -----------------------------------------
# library(curl)
# library(httr)

get_genre <- function(cleaned_books,i){
  isbn = cleaned_books$isbn[i]
  if(isbn =='') return (NA) else{
    r <- GET(paste('https://openlibrary.org/api/books?bibkeys=ISBN:',isbn,'&jscmd=data&format=json',sep=''))
    #r
    #headers(r)
    #content(r, "text")
    #str(content(r, "parsed"))
    if(length((content(r, "parsed")))==0){return(NA)}else{
      subjlist <- content(r, "parsed")[[1]]$subjects
      genres <- unlist(map(subjlist,'name'))
      return(genres)
    }}
}


genrelist <- list()
s <- Sys.time()
for(i in 1:nrow(cleaned_books)){
  genrelist[[i]] <- get_genre(cleaned_books = cleaned_books,i = i)
  print(i)
  print(genrelist[i])
}
st <- Sys.time()
print(st - s)

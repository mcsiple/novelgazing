# Shortest, longest, top authors, etc

sltable <- function(cleaned_books){
  read <- cleaned_books %>% 
    filter(exclusive_shelf=='read')
  longest <- read %>%
    filter(number_of_pages==max(number_of_pages,na.rm=T))
  shortest <- read %>%
    filter(number_of_pages==min(number_of_pages,na.rm=T))
  oldest <- read %>%
    filter(original_publication_year == min(original_publication_year,na.rm=T))
  df <- data.frame("."=c("Longest book you've ever read",
                         "Shortest book you've ever read",
                         "Oldest book you've ever read"),
                   Title = c(paste(longest$title),
                             paste(shortest$title),
                             paste(oldest$title)),
                   Author = c(paste(longest$author),
                              paste(shortest$author),
                              paste(oldest$author)),
                   ".." = c(paste0("Number of pages: ",longest$number_of_pages),
                            paste0("Number of pages: ",shortest$number_of_pages),
                            paste0("Date of publication: ",oldest$original_publication_year)))
  return(df)
}

top3authors <- function(cleaned_books, ranking = 'count'){
  #ranking is either by how many books you've read or what your ratings are, count or rating
  read <- cleaned_books %>% 
    filter(exclusive_shelf=='read')
  unread <- cleaned_books %>% 
    filter(exclusive_shelf=='to-read')
  topdf <- cleaned_books %>%
    group_by(exclusive_shelf,author) %>%
    summarize(authorcount = length(author),
              authorrating = mean(my_rating,na.rm=T))
  if(ranking == 'count'){
    x <- topdf %>% 
      filter(exclusive_shelf=='read') %>%
      ungroup() %>%
      arrange(desc(authorcount)) %>%
      top_n(authorcount,n = 3) %>%
      select(author,authorcount) %>%
      rename("Number of books you've read by this author" = authorcount)
    # want top 3 authors by number of books you've read
  }else{
    x <- topdf %>%
      filter(exclusive_shelf=='read') %>%
      ungroup() %>%
      arrange(desc(authorrating)) %>%
      top_n(authorrating,n = 3) %>%
      select(author,authorrating) %>%
      rename("Your average rating of this author's books (out of 5)" = authorrating)
  }
  if(nrow(x)>3){x <- x[1:3,]}
  x <- as.data.frame(x)
  return(x)
}


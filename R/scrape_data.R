# This code is based on Mara Averick's awesome goodreads part 2 blog post
# https://maraaverick.rbind.io/2017/10/goodreads-part-2/
# I added a few pieces of info including the 'community matrix'

library(tidyverse)
library(stringr)
library(rvest) # for cool html stuff
library(lubridate)
library(curl) #for id'ing agent to server


startUrl <- "https://www.goodreads.com/review/list/8200244-megsie"

# function to get book descriptions
getBookDescription <- function(bookLink) {
  url <- str_c("https://www.goodreads.com", bookLink)
  read_html(url) %>%
    html_node("#descriptionContainer") %>%
    html_text() %>%
    trimws()
}

# function to get book genres
get_genres <- function(bookLink){
  url <- str_c("https://www.goodreads.com", bookLink)
  read_html(url) %>%
    html_nodes(".left .bookPageGenreLink") %>%
    html_text(trim = TRUE)
}

# function to get books
getBooks <- function(i) {
  #cat(i, "\n")
  url <- str_c(startUrl, "?page=", i, "&shelf=read")
  
  #html <- read_html(url)
  html <- read_html(curl(url,handle = curl::new_handle("useragent" = "Mozilla/5.0")))
  title <- html %>%
    html_nodes(".title a") %>%
    html_text(trim = TRUE)
  
  author <- html %>%
    html_nodes(".author a") %>%
    html_text(trim = TRUE) %>%
    discard(str_detect(., "^\\("))
  
  bookLinks <- html %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    discard(!str_detect(., "^/book/show")) %>%
    na.omit() %>%
    unique()
  
  bookDescription <- bookLinks %>%
    map_chr(getBookDescription)
  
  bookGenre <- bookLinks %>%
    map(get_genres)
  
  yearRead1 <- html %>% # mcs
    html_nodes(".date_read_value") %>%
    html_text(trim = TRUE) %>%
    mdy() %>%
    year()
  
  yearRead2 <- html %>% 
    html_nodes(".date_read") %>%
    map_df(~list(date_read = html_nodes(.x, '.date_read_value') %>% 
                   html_text(trim = TRUE) %>% 
                   {if(length(.) == 0) NA else .})) %>%  #Christine genius
    slice(2:n()) %>% 
    mutate(newdate = mdy(date_read),
           year = year(newdate)) %>%
    select(year)
  
  if(length(yearRead1)==length(author)){
    yearRead = yearRead1}else{
      yearRead = yearRead2$year}
  
  #Sys.sleep(5)
  
  return(
    tibble(
      title = title,
      author = author,
      book_description = bookDescription,
      book_genres = bookGenre,
      year_read = yearRead
    )
  )
}

getnbooks <- function(){
  url <- str_c(startUrl, "?page=", 1, "&shelf=read")
  html <- read_html(url)
  nbooks <- html %>%
    html_nodes('title') %>%
    html_text(trim = TRUE) %>%
    str_extract("[[:digit:]]+") %>%
    as.numeric()
  return(nbooks)
}

# get books (this takes a while!)
(npages <- ceiling(getnbooks()/30) ) # 30 entries per page

goodreads <- map_df(1:npages, ~{
  Sys.sleep(10) # don't timeout the goodreads server
  cat(.x)
  getBooks(.x)
})

# save xls
goodreads %>% 
  select(-book_genres) %>%
write_csv(path = here('output', 
                      'goodreads_read.csv'))

# save genre data
genres <- goodreads %>%
  select(book_genres,year_read) %>%
  mutate(id = 1:n()) %>%
  unnest_longer(book_genres)
  
# Turn genres into 'community matrix'
community <-  genres %>% 
              pivot_longer(c(-id,-year_read)) %>% 
              dplyr::count(year_read, value) %>% # to give each book its own diversity, change to dplyr::count(id, value)
              pivot_wider(
                names_from = value, 
                values_from = n, 
                values_fill = list(n = 0)
                ) %>%
                as.data.frame()

write_csv(community, path = here('output','communitymatrix.csv'))




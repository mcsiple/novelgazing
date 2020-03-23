# This code is based on Mara Averick's awesome goodreads part 2 blog post
# https://maraaverick.rbind.io/2017/10/goodreads-part-2/
# I added a few pieces of info including the 'community matrix'

# library(tidyverse)
# library(stringr)
# library(rvest) # for cool html stuff
# library(lubridate)
# library(curl) #for id'ing agent to server
# 
# usernumber <- 8200244
# username <- 'megsie'
# startUrl <- paste('https://www.goodreads.com/review/list/',usernumber,'-',username,sep='')
# startUrl <- paste('https://www.goodreads.com/review/list/','29005117','-','federico-dn',sep='')


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
get_books <- function(i,stUrl) {
  #cat(i, "\n")
  url <- str_c(stUrl, "?page=", i, "&shelf=read")
  
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
  
  yearRead <- html %>%
    html_nodes('.date_read') %>%
    map_df(~list(date_read = html_nodes(.x, '.date_row') %>% 
                   html_text(trim = TRUE) %>% 
                   {if(length(.) > 1) extract2(.,1) else .} %>%
                   str_replace_all(., 'not set', NA_character_))) %>%
    mutate(newdate = mdy(date_read),
           year = year(newdate)) %>%
    select(year)
    
  monthRead <- html %>%
    html_nodes('.date_read') %>%
    map_df(~list(date_read = html_nodes(.x, '.date_row') %>% 
                   html_text(trim = TRUE) %>% 
                   {if(length(.) > 1) extract2(.,1) else .} %>%
                   str_replace_all(., 'not set', NA_character_)))%>%
    mutate(newdate = mdy(date_read),
           month = month(newdate)) %>%
    select(month)
  
  #sometimes there are NA years and you have to do these hijinx
  # yearRead1 <- html %>% # mcs
  #   html_nodes(".date_read_value") %>%
  #   html_text(trim = TRUE) %>%
  #   mdy() %>%
  #   year()
  # 
  # nodes_megsie <- function(x){
  #   y <- html.nodes(x,'.date_read_value') 
  # }
  # 
  # yearRead2 <- html %>% 
  #   html_nodes(".date_read") %>%
  #   map_df(~list(date_read = html_nodes(.x, '.date_read_value') %>% 
  #                  html_text(trim = TRUE) %>% 
  #                  {if(length(.) == 0) NA else .})) %>%  #Christine genius
  #   slice(2:n()) %>% 
  #   mutate(newdate = mdy(date_read),
  #          year = year(newdate)) %>%
  #   select(year)
  # 
  # if(length(yearRead1)==length(author)){
  #   yearRead = yearRead1}else{
  #     yearRead = yearRead2$year}
  # 
  # monthRead <- html %>% # mcs
  #   html_nodes(".date_read_value") %>%
  #   html_text(trim = TRUE) %>%
  #   mdy() %>%
  #   month()
  # 
  # monthRead1 <- html %>% # mcs
  #   html_nodes(".date_read_value") %>%
  #   html_text(trim = TRUE) %>%
  #   mdy() %>%
  #   month()
  # 
  # monthRead2 <- html %>% 
  #   html_nodes(".date_read") %>%
  #   map_df(~list(date_read = html_nodes(.x, '.date_read_value') %>% 
  #                 # magrittr::extract2(1) %>%
  #                  html_text(trim = TRUE) %>% 
  #                  {if(length(.) == 0) NA else .})) %>%  #Christine genius
  #   slice(2:n()) %>% 
  #   mutate(newdate = mdy(date_read),
  #          month = month(newdate)) %>%
  #   select(month)
  # 
  # if(length(monthRead1)==length(author)){
  #   monthRead = monthRead1}else{
  #     monthRead = monthRead2$month}
  
#print(yearRead)
#print(monthRead)
#print(author)
  
  return(
    tibble(
      title = title,
      author = author,
      book_description = bookDescription,
      book_genres = bookGenre,
      year_read = yearRead,
      month_read = monthRead
  )
  )
}

getnbooks <- function(stUrl=startUrl){
  url <- str_c(stUrl, "?page=", 1, "&shelf=read")
  html <- read_html(url)
  nbooks <- html %>%
    html_nodes('title') %>%
    html_text(trim = TRUE) %>%
    str_extract("[[:digit:]]+") %>%
    as.numeric()
  return(nbooks)
}

# get books (this takes a while!)
#(npages_in <- ceiling(getnbooks(stUrl = startUrl)/30) ) # 30 entries per page
# This function is in operation, it's just now in the app instead of as a separate function
scrape_goodreads <- function(npages = npages_in){
  goodreads <- map_df(1:npages, ~{
    Sys.sleep(5) # don't timeout the goodreads server
    cat(.x)
    get_books(.x,stUrl = startUrl)
  })
  return(goodreads)
}

# get big fun matrix for further analysis
get_cmatrix_and_genres <- function(scraped_data){
  goodreads <- scraped_data
  goodreads_read <- goodreads %>%
    select(-book_genres)
  #write_csv(path = here('output',
  #                      'goodreads_read.csv'))
  
  # save genre data
  genres <- goodreads %>%
    select(book_genres,year_read) %>%
    mutate(id = 1:n()) %>%
    unnest_longer(book_genres)
  
  # save genres by month
  genres_month <- goodreads %>%
    select(book_genres,month_read) %>%
    mutate(id = 1:n()) %>%
    unnest_longer(book_genres)
  
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
  return(list(community = community,
              genres = genres,
              genres_month = genres_month,
              goodreads_read = goodreads_read))
}



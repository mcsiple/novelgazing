# GOODREADS API KEY
# Here is your developer key for using the Goodreads API. This key must be appended to every request using the form variable 'key'. (If you're using our write API, you'll need your secret too.)
library(httr)

# The following code is adapted from https://www.r-bloggers.com/goodreads-api/

# key: Gvtj9GhGoMj7EU903pNkZw
# secret: 6cGXBK2RoVFl5PTD8DCsbBcZK1fUl7vm0wQeGdnM9s
#Sys.setenv(GOODREADS_KEY = "Gvtj9GhGoMj7EU903pNkZw")
api <- "Gvtj9GhGoMj7EU903pNkZw"

library(httr)
library(rgoodreads)

# # Data thru API is limited
usr_id <- '8200244'
urlg <- "https://www.goodreads.com/review/list?"

#nyssa-j-silbiger
usr_id <- '30279160'

#

get_shelf <- function(GR_ID, url, shelf = 'read') {
  shelf <- GET(url, query = list(
    v = 2, key = api, id = GR_ID, shelf = shelf, per_page = 200))
  shelf_contents <- content(shelf, as = "parsed")
  return(shelf_contents)
}

shelf <- get_shelf(usr_id, url = urlg,shelf = 'read')
shelf2 <- get_shelf(usr_id, url = urlg,shelf = 'to-read')


get_df <- function(shelf) {
  
  title <- shelf %>% 
    xml_find_all("//title") %>% 
    xml_text()
  
  rating <- shelf %>% 
    xml_find_all("//rating") %>% 
    xml_text()
  
  avg_rating <- shelf %>%
    xml_find_all("//average_rating") %>% 
    xml_text()
  avg_rating <- avg_rating[c(TRUE,FALSE)] # Get odd indices
  # Don't know why but the above function extracts 2x the ratings, and every other rating is the actual one for that book *shrug emoji*
  isbn <- shelf %>%
    xml_find_all("//") %>% 
    xml_text()
  
  added <- shelf %>% 
    xml_find_all("//date_added") %>% 
    xml_text()
  
  started <- shelf %>% 
    xml_find_all("//started_at") %>% 
    xml_text()
  
  read <- shelf %>% 
    xml_find_all("//read_at") %>% 
    xml_text()
  
  description <- shelf %>% 
    xml_find_all("//description") %>% 
    xml_text()
  
  df <- tibble(
    title, rating, avg_rating, added, started, read)
  
  return(df)
}

df <- get_df(shelf)





# Cleanup stuff (might not need) ------------------------------------------
get_books <- function(df) {
  
  books <- df %>% 
    gather(date_type, date, -title, -rating) %>% 
    separate(date, 
             into = c("weekday", "month", "day", "time", "zone", "year"), 
             sep = "\\s", fill = "right") %>% 
    mutate(date = str_c(year, "-", month, "-", day)) %>% 
    select(title, rating, date_type, date) %>% 
    mutate(date = as.Date(date, format = "%Y-%b-%d")) %>% 
    spread(date_type, date) %>% 
    mutate(title = str_replace(title, "\\:.*$|\\(.*$|\\-.*$", "")) %>% 
    mutate(started = ifelse(
      is.na(started), as.character(added), as.character(started))) %>% 
    mutate(started = as.Date(started)) %>% 
    mutate(rating = as.integer(rating))
  
  return(books)
}

#books <- get_books(df)



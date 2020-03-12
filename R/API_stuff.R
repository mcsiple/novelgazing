# GOODREADS API KEY
# Here is your developer key for using the Goodreads API. This key must be appended to every request using the form variable 'key'. (If you're using our write API, you'll need your secret too.)
# 
# key: Gvtj9GhGoMj7EU903pNkZw
# secret: 6cGXBK2RoVFl5PTD8DCsbBcZK1fUl7vm0wQeGdnM9s
Sys.setenv(GOODREADS_KEY = "Gvtj9GhGoMj7EU903pNkZw")
api <- "Gvtj9GhGoMj7EU903pNkZw"
#devtools::install_github("famguy/rgoodreads")
library(httr) #dependency for rgoodreads
library(rgoodreads)
# # Data thru API is limited
usr_id <- '8200244'
url <- "https://www.goodreads.com/review/list?"

glimpse(my_data)
my_data <- user(usr_id)

get_shelf <- function(GR_ID) {
  shelf <- GET(url, query = list(
    v = 2, key = api, id = GR_ID, shelf = "read", per_page = 200))
  shelf_contents <- content(shelf, as = "parsed")
  return(shelf_contents)
}
shelf <- get_shelf(usr_id)

get_df <- function(shelf) {
  
  title <- shelf %>% 
    xml_find_all("//title") %>% 
    xml_text()
  
  rating <- shelf %>% 
    xml_find_all("//rating") %>% 
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
  
  df <- tibble(
    title, rating, added, started, read)
  
  return(df)
}

df <- get_df(shelf)

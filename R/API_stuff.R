# GOODREADS API KEY
# Here is your developer key for using the Goodreads API. This key must be appended to every request using the form variable 'key'. (If you're using our write API, you'll need your secret too.)
# 
# key: Gvtj9GhGoMj7EU903pNkZw
# secret: 6cGXBK2RoVFl5PTD8DCsbBcZK1fUl7vm0wQeGdnM9s
# The initial code is from Mara Averick's blog here: https://maraaverick.rbind.io/2017/10/goodreads-part-2/
Sys.setenv(GOODREADS_KEY = "Gvtj9GhGoMj7EU903pNkZw")
devtools::install_github("famguy/rgoodreads")
library(httr) #dependency for rgoodreads
library(rgoodreads)
# # Data thru API is limited
my_data <- user('8200244')
glimpse(my_data)



# devtools::install_github("gadenbuie/ggpomological")

#library(rgoodreads)
#library(httr)  
library(lubridate)
library(here)
library(tidyverse)
library(ggpomological)
library(reshape2)

# Load your own user data separately
my_books <- read.csv(here::here('data','goodreads_library_export.csv'),
                       header=T)
library(snakecase)


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
  

# book pages
read_books %>%
  ggplot(aes(year_read)) +
  geom_bar(stat = "count") +
  labs(title = "Books read per year", 
       caption = "source: Megsie's goodreads") +
  scale_x_continuous(breaks=seq(2009,2020,by=2),
                     labels=seq(2009,2020,by=2))


criticplot <- read_books %>% 
  group_by(year_read) %>% summarize(my_mean_rating = mean(my_rating),
                                    allusers_mean_rating=mean(average_rating)) %>%
  ggplot(aes(x=year_read)) +
  geom_segment(aes(y= allusers_mean_rating,yend=my_mean_rating,xend=year_read),
               col="darkgrey",lwd=0.5) + #,arrow = arrow(length = unit(0.2, "cm"))
  geom_point(aes(x=year_read,y=allusers_mean_rating),colour="grey",size=3) + 
  geom_point(aes(x=year_read,y=my_mean_rating),colour="#d95f0e",size=3) + #blue indicates my rating
  ylab("Average rating") +
  xlab("Year") +
  scale_x_continuous(breaks=seq(2009,2020,by=2),
                     labels=seq(2009,2020,by=2)) +
  ggtitle("Has Megsie become a more critical reader?")


criticplot + theme_pomological("Homemade Apple", 16)

# basic_diagnostics
# For basic page, take csv data and produce:


library(lubridate)
library(here)
library(tidyverse)
library(ggpomological)
library(reshape2)
library(snakecase)
library(patchwork)

# Load your own user data separately

my_books <- read.csv(here::here('data','goodreads_library_export.csv'),
                     header=T) # raw data frame

basic_diagnostics <- function(my_books){
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
  
  BPY <- read_books %>%
    ggplot(aes(year_read)) +
    geom_bar(stat = "count") +
    xlab('Year') +
    ylab('Count') +
    labs(title = "Books read per year", 
         caption = "source: Goodreads") +
    scale_x_continuous(breaks=seq(2009,2020,by=2),
                       labels=seq(2009,2020,by=2))
  
  criticplot <- read_books %>% 
    group_by(year_read) %>% 
    summarize(my_mean_rating = mean(my_rating),
              allusers_mean_rating=mean(average_rating)) %>%
    ggplot(aes(x=year_read)) +
    geom_segment(aes(y= allusers_mean_rating,yend=my_mean_rating,xend=year_read),
                 col="darkgrey",lwd=0.5) +
    geom_point(aes(x=year_read,y=allusers_mean_rating),colour="grey",size=3) + 
    geom_point(aes(x=year_read,y=my_mean_rating),colour="#d95f0e",size=3) +
    ylab("Average rating") +
    xlab("Year") +
    scale_x_continuous(breaks=seq(2009,2020,by=2),
                       labels=seq(2009,2020,by=2)) +
    ggtitle("Have you become a more critical reader?") +
    ggsidekick::theme_sleek(base_size=14)
  
  criticplot
}
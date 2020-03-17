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
testfile <- here::here('data','goodreads_library_export.csv')

read_books <- cleanup_csv(csvfile = testfile)

basic_diagnostics <- function(read_books){
  bbreaks <- seq(min(read_added$year,na.rm=T),
                 max(read_added$year,na.rm=T),by = 2)
  
  BPY <- read_books %>%
    ggplot(aes(year_read)) +
    geom_bar(stat = "count") +
    xlab('Year') +
    ylab('Count') +
    labs(title = "Books read per year", 
         caption = "source: Goodreads") +
    scale_x_continuous(breaks=bbreaks,
                       labels=bbreaks)
  
  criticplot <- read_books %>% 
    group_by(year_read) %>% 
    summarize(my_mean_rating = mean(my_rating),
              allusers_mean_rating=mean(average_rating)) %>%
    ggplot(aes(x=year_read)) +
    geom_segment(aes(y=allusers_mean_rating,
                     yend=my_mean_rating,
                     xend=year_read),
                 col="darkgrey",lwd=0.5) +
    geom_point(aes(x=year_read,y=allusers_mean_rating),colour="grey",size=3) + 
    geom_point(aes(x=year_read,y=my_mean_rating),colour="#d95f0e",size=3) +
    ylab("Average rating") +
    xlab("Year") +
    scale_x_continuous(breaks=bbreaks,
                       labels=bbreaks) +
    ggtitle("Have you become a more critical reader?") +
    ggsidekick::theme_sleek(base_size=14)
  
   x <- read_books %>%
    group_by(year_added) %>%
    summarize(n1 = length(title)) %>%
              mutate(cn = cumsum(n1),
                     AR = 'Added') %>%
     rename(year = year_added)
    y <- read_books %>%
     group_by(year_read) %>%
     summarize(n2 = length(title)) %>%
      mutate(cn = cumsum(n2),
             AR = 'Finished') %>%
      rename(year = year_read)
  
    read_added <- bind_rows(x,y)
    
    cumubooks <- 
      read_added %>%
      ggplot(aes(x = year,y = cn,fill = AR)) + 
      geom_area() +
      scale_x_continuous(breaks = bbreaks) +
      ggsidekick::theme_sleek() +
      scale_fill_manual('',values = ghibli::ghibli_palette('YesterdayLight')[5:6]) +
      labs(title = 'Cumulative books finished or added') +
      xlab('Year') +
      ylab('Number of books')
  
    
    
}
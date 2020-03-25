# basic_diagnostics
# For basic page, take csv data and produce:

#library(lubridate)
#library(here)
#library(tidyverse)
#library(ggpomological)
#library(reshape2)
#library(snakecase)
#library(patchwork)

# Load your own user data separately
#testfile <- here::here('data','goodreads_library_export.csv')
#cleaned_csv1 <- cleanup_csv(read.csv(testfile,header = TRUE))
  

basic_diagnostics <- function(cleaned_csv = cleaned_csv1,
                              pal = bookpal){
  read_books <- cleaned_csv
  bbreaks <- seq(min(read_books$year_added,na.rm=T),
                 max(read_books$year_added,na.rm=T),by = 2)
  
  BPY <- read_books %>%
    ggplot(aes(year_read)) +
    geom_bar(stat = "count",fill=pal[6]) +
    xlab('Year') +
    ylab('Books') +
    # labs(#title = "Books per year", 
    #      caption = "source: Goodreads") +
    scale_x_continuous(breaks=bbreaks,
                       labels=bbreaks) +
    ggsidekick::theme_sleek(base_size = 12)
  
  PPY <- read_books %>%
    ggplot(aes(x = year_read, y = sum(number_of_pages,na.rm=T))) +
    geom_col(fill = pal[6]) +
    xlab('Year') +
    ylab('Pages') +
    scale_x_continuous(breaks=bbreaks,
                       labels=bbreaks) +
    ggsidekick::theme_sleek(base_size = 12)
  
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
    geom_point(aes(x=year_read,y=my_mean_rating),colour=pal[1],size=3) +
    ylab("Average rating") +
    xlab("Year") +
    scale_x_continuous(breaks=bbreaks,
                       labels=bbreaks) +
    labs(title = "Have you become a more critical reader?",
         caption = "source: Goodreads") +
    ggsidekick::theme_sleek(base_size=12) +
    theme(legend.position = 'bottom')
  
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
    geom_area(size=0.5,colour='white') +
    scale_x_continuous(breaks = bbreaks) +
    ggsidekick::theme_sleek(base_size = 12) +
    scale_fill_manual('',values = pal[3:4]) +
    xlab('Year') +
    ylab('Cumulative books finished or added') +
    theme(legend.position = 'bottom')
  
  (BPY | PPY) / (cumubooks | criticplot)
}

#basic_diagnostics(cleaned_csv = cleaned_csv1,pal = bookpal)

time_to_finish_shelves <- function(cleaned_csv = cleaned_csv1,
                           pal = bookpal){
  read_books <- cleaned_csv
  x <- read_books %>% 
    group_by(exclusive_shelf) %>%
    summarize(n = length(title)) 
  
  nread <- x %>%
    filter(exclusive_shelf=='read') %>%
    select(n) %>%
    as.numeric()
  
  n2read <- x %>%
    filter(exclusive_shelf=='to-read') %>%
    select(n) %>%
    as.numeric()
  
  nyears <- max(read_books$year_read,na.rm=T) - min(read_books$year_added,na.rm=T)
  bpy <- nread/nyears
  print(nread)
  print(bpy)
  print(n2read)
  nyears_to_finish <- n2read/bpy
  
  #Plot 
  plotdat <- read_books %>%
    filter(exclusive_shelf == 'read') %>%
    mutate(ttr = year_read - year_added) %>%
    mutate(title = fct_reorder(title, ttr, max, .desc = TRUE)) %>%
    top_n(6) %>%
    arrange(desc(-ttr)) 
  
  ibp <- plotdat %>%
    ggplot(aes(title,ttr)) +
    geom_col(fill = pal[1]) +
    ggsidekick::theme_sleek(base_size = 16) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
    xlab("Books you waited longest to read") +
    ylab("Years between adding the book \nto your shelf and reading it") +
    coord_flip()
  
 # if(all(is.na(plotdat$ttr))){ibp <- ibp + annotate()}
  
    
  return(list(nyears_to_finish = nyears_to_finish,
              ind_books_plot = ibp))
}
#time_to_finish_shelves(cleaned_csv = cleaned_csv1,pal = bookpal)

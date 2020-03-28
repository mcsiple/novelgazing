# basic_diagnostics
# For basic page, take csv data and produce:

# library(lubridate)
# library(here)
# library(tidyverse)
# library(reshape2)
# library(snakecase)
# library(patchwork)

# Load your own user data separately
#testfile <- here::here('data','goodreads_library_export_k.csv')
#cleaned_csv1 <- cleanup_csv(read.csv(testfile,header = TRUE))
  

basic_diagnostics <- function(cleaned_csv = cleaned_csv1,
                              pal = bookpal){
  read_books <- cleaned_csv %>% 
    filter(exclusive_shelf=='read')
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
    ggsidekick::theme_sleek(base_size = 14)
  
  PPY <- read_books %>%
    group_by(year_read) %>%
    summarize(pgs = sum(number_of_pages,na.rm=T)) %>%
    ggplot(aes(x = year_read,
               y = pgs)) +
    geom_col(fill = pal[6]) +
    xlab('Year') +
    ylab('Pages') +
    scale_x_continuous(breaks=bbreaks,
                       labels=bbreaks) +
    ggsidekick::theme_sleek(base_size = 14)
  
  criticplot <- read_books %>% 
    group_by(year_read) %>% 
    summarize(my_mean_rating = mean(my_rating),
              allusers_mean_rating=mean(average_rating)) %>%
    mutate(ratingdiff = my_mean_rating-allusers_mean_rating,
           above = ifelse(ratingdiff>0,'Above average','Below average')) %>%
    ggplot() +
    geom_hline(yintercept = 0,lty=2,colour='grey50') +
    geom_segment(aes(x=year_read,y=0, yend = ratingdiff,xend=year_read),
                 colour = 'grey50',lwd=.8) +
    geom_point(aes(x=year_read,y=ratingdiff,colour = above),size=3) +
    scale_colour_manual('',values = pal[c(1,2)]) +
    ylab("Your ratings relative to \n the Goodreads community") +
    xlab("Year") +
    scale_x_continuous(breaks=bbreaks,
                       labels=bbreaks) +
    labs(caption = "source: Goodreads") +
    ggsidekick::theme_sleek(base_size=14) +
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
    ggsidekick::theme_sleek(base_size = 14) +
    scale_fill_manual('',values = pal[3:4]) +
    xlab('Year') +
    ylab('Cumulative books \nfinished or added') +
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
  nyears_to_finish <- round(n2read/bpy, digits = 2)
  
  #Plot 
  plotdat <- read_books %>%
    filter(exclusive_shelf == 'read') %>%
    mutate(ttr = year_read - year_added) %>%
    mutate(title = fct_reorder(title, ttr, max, .desc = TRUE)) %>%
    top_n(5,wt = ttr) 
  if(nrow(plotdat)>6) plotdat <- plotdat[1:6,]
  
  emptylabel <- "If there are no orange bars here, you haven't added a book before reading it - prompt!"
  wrapper <- function(x, ...) paste(strwrap(x, ...), collapse = "\n")
  
  ibp <- plotdat %>%
    ggplot(aes(title,ttr)) +
    geom_col(fill = pal[1]) +
    ggsidekick::theme_sleek(base_size = 16) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
    xlab("Books you waited longest to read") +
    ylab("Years between adding the book \nto your shelf and reading it") +
    coord_flip() 
  #ibp
 
  if(all(is.na(plotdat$ttr))){ibp <- ibp + 
    annotate("label",
             x = 5,y = 2,
             label = wrapper(emptylabel,width = 20),
             family = "",
             fontface = 1, size=4)}
  
    
  return(list(nyears_to_finish = nyears_to_finish,
              ind_books_plot = ibp))
}
#time_to_finish_shelves(cleaned_csv = cleaned_csv1,pal = bookpal)

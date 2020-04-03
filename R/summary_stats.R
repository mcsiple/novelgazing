# Shortest, longest, oldest

sltable <- function(cleaned_books){
  read <- cleaned_books %>% 
    filter(exclusive_shelf=='read')
  longest <- read %>%
    filter(number_of_pages==max(number_of_pages,na.rm=T)) %>%
    slice(1)
  shortest <- read %>%
    filter(number_of_pages==min(number_of_pages,na.rm=T))%>%
    slice(1)
  oldest <- read %>%
    filter(original_publication_year == min(original_publication_year,na.rm=T))%>%
    slice(1)
  df <- data.frame("."=c("Longest book you've ever read",
                         "Shortest book you've ever read",
                         "Oldest book you've ever read"),
                   Title = c(paste(longest$title),
                             paste(shortest$title),
                             paste(oldest$title)),
                   Author = c(paste(longest$author),
                              paste(shortest$author),
                              paste(oldest$author)),
                   "Information" = c(paste0("Number of pages: ",longest$number_of_pages),
                            paste0("Number of pages: ",shortest$number_of_pages),
                            paste0("Date of publication: ",oldest$original_publication_year)))
  return(df)
}

# Top authors by ranking and by number of books finished
top3authors <- function(cleaned_books, ranking = 'count'){
  #ranking is either by how many books you've read or what your ratings are, count or rating
  read <- cleaned_books %>% 
    filter(exclusive_shelf=='read')
  unread <- cleaned_books %>% 
    filter(exclusive_shelf=='to-read')
  topdf <- cleaned_books %>%
    group_by(exclusive_shelf,author) %>%
    summarize(authorcount = length(author),
              authorrating = mean(my_rating,na.rm=T))
  if(ranking == 'count'){
    x <- topdf %>% 
      filter(exclusive_shelf=='read') %>%
      ungroup() %>%
      arrange(desc(authorcount)) %>%
      top_n(authorcount,n = 3) %>%
      select(author,authorcount) %>%
      rename("Number of books you've read \n by this author" = authorcount,
             "Author" = author)
    # want top 3 authors by number of books you've read
  }else{
    x <- topdf %>%
      filter(exclusive_shelf=='read') %>%
      ungroup() %>%
      arrange(desc(authorrating)) %>%
      top_n(authorrating,n = 3) %>%
      select(author,authorrating) %>%
      rename("Your average rating \n of this author's books (out of 5)" = authorrating,
             "Author" = author)
  }
  if(nrow(x)<1){x <- NA}
  if(nrow(x)>3){x <- x[1:3,]}
  x <- as.data.frame(x)
  return(x)
}


monthfigure <- function(cleaned_csv){
  read_books <- cleaned_csv %>%
    filter(exclusive_shelf=='read') %>%
    mutate(month_read = lubridate::month(date_read,label=TRUE),
           month_added = lubridate::month(date_added,label=TRUE))
  x <- read_books %>%
    filter(!is.na(month_added)) %>%
    group_by(month_added) %>%
    summarize(n = length(title)) %>%
    mutate(AR = 'Added') %>%
    ungroup() %>%
    mutate(month = month_added)%>%
    filter(!is.na(month)) %>%
    mutate(maxmonth = ifelse(n==max(n,na.rm=T),AR,NA))
  
  y <- read_books %>%
    filter(!is.na(month_read)) %>%
    group_by(month_read) %>%
    summarize(n = length(title)) %>%
    mutate(AR = 'Finished') %>%
    ungroup() %>%
    mutate(month = month_read) %>%
    filter(!is.na(month)) %>%
    mutate(maxmonth = ifelse(n==max(n,na.rm=T),AR,NA))
    
    read_added <- bind_rows(x,y) 
  
  
  dotplot <- read_added %>% 
            ggplot() +
            geom_point(aes(x=month,y=AR,colour=AR,size=n),alpha=0.8) +
            scale_colour_manual(values = pal[4:5]) +
            scale_size('N',range = c(1,20)) +
            ggsidekick::theme_sleek(base_size = 14) +
            xlab('Month') +
            ylab('') +
            guides(colour = FALSE,
                   size = guide_legend(override.aes = list(colour='lightgrey'))) +
            geom_point(aes(x=month,y=maxmonth),na.rm=T,pch=8) +
    labs(caption = '* indicates maximum books')
  return(dotplot)
}



likeplot <- function(cleaned_csv){
  read_books <- cleaned_csv %>%
    filter(exclusive_shelf=='read') 
  
fb <- read_books %>%
  select(title,my_rating,average_rating) %>%
  mutate(fave = ifelse(my_rating>average_rating*1.25,1,0),
         unfave = ifelse(my_rating !=0 & my_rating<.5*average_rating,1,0)) %>%
  pivot_longer(cols=c('fave','unfave')) %>%
  filter(value==1) %>%
  group_by(name)

if(nrow(fb %>% filter(name=='unfave')) < 2 | nrow(fb %>% filter(name=='fave'))<2){
  favebooks <- fb %>% sample_n(size=1) }else{
  favebooks <- fb %>% sample_n(size = 2)
}


lk <- read_books %>%
  ggplot(aes(x=my_rating,y=average_rating)) +
  geom_point(position = position_jitter(w = 0.2, h = 0), alpha=0.5,colour='grey50',size = 4) +
  geom_abline(intercept = 0,lty = 2) +
  ggsidekick::theme_sleek(base_size=18) +
  geom_point(data=favebooks,aes(x=my_rating,y=average_rating,colour=factor(name)),size = 4) +
  scale_colour_manual('',values = c("#7DF4FF","#FF6E76"))

likeplot <- lk  + geom_label_repel(data = filter(favebooks,name=='fave'),
                                aes(x=my_rating,y=average_rating,label=title),
                                point.padding = 0,
                                force=30,
                                box.padding = 0.8,
                                nudge_x = -0.2,
                                nudge_y = -2,
                                segment.color = 'grey50'
                                ) +
  geom_label_repel(data = filter(favebooks,name=='unfave'),
                   aes(x=my_rating,y=average_rating,label=title),
                   point.padding = 0,
                   force=30,
                   box.padding = 0.8,
                   nudge_y = 1,
                   nudge_x = 0.7,
                   segment.color = 'grey50'
                   ) +
  xlim(c(0,6)) +
  ylim(c(0,6)) +
  xlab('Your rating') +
  ylab('Average community rating') +
  annotate(geom = 'text',x = 1.5,y=6,size = 5,label = 'Books you liked less than average') +
  annotate(geom = 'text',x = 4.5,y=0,size = 5,label = 'Books you liked more than average') +
  theme(legend.position = 'none') +
  labs(caption='Colorful points are a random sample \n from your most loved and most hated books')
print(favebooks)
return(likeplot)
}


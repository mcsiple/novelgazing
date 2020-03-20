# genre diversity over time
# 
get_rainbow_plot <- function(scraped_data,pal,whichplot){
  #whichplot is 'yearly' or 'monthly'
  g <- get_cmatrix_and_genres(scraped_data = scraped_data)$genres
  
  allgenres <- g %>%
    group_by(year_read,book_genres) %>%
    summarize(total = length(book_genres))  %>%
    ungroup(year_read,book_genres) %>%
    mutate(year_read = as.integer(year_read))
  
  top10genres <- g %>%
    group_by(book_genres) %>%
    summarize(bcount = length(book_genres)) %>%
    arrange(desc(bcount)) %>%
    top_n(bcount, n = 10)
  
  pdat <- allgenres %>%
    mutate(simple_genre = ifelse(book_genres %in% top10genres$book_genres,book_genres,'Other')) %>%
    group_by(year_read,simple_genre) %>%
    ungroup(simple_genre) %>%
    mutate(simple_genre = fct_relevel(simple_genre, 'Other', after = Inf))
  nb.cols <- length(levels(pdat$simple_genre))
  mycolors <- colorRampPalette(bookpal)(nb.cols)
  mycolors[levels(pdat$simple_genre)=='Other'] = 'gray'
  
  rainbowbeauty <- pdat %>% 
    ggplot(aes(year_read,total,fill=simple_genre)) + 
    geom_col() +
    scale_fill_manual(values = mycolors) +
    xlab('Year') +
    ylab('Total count among books you finished') +
    ggsidekick::theme_sleek(base_size = 14) +
    labs(caption = "*Note: Most books have cross-referenced,\n so the genre total will be higher than the number of books you read.")
  
  g2 <- get_cmatrix_and_genres(scraped_data = scraped_data)$genres_month
  
  allgenres_month <- g2 %>%
    group_by(month_read,book_genres) %>%
    summarize(total = length(book_genres))  %>%
    ungroup(year_read,book_genres) %>%
    mutate(month_read = as.integer(month_read))
  
  pdat2 <- allgenres_month %>%
    mutate(simple_genre = ifelse(book_genres %in% top10genres$book_genres,book_genres,'Other')) %>%
    group_by(month_read,simple_genre) %>%
    ungroup(simple_genre) %>%
    mutate(simple_genre = fct_relevel(simple_genre, 'Other', after = Inf))
  nb.cols <- length(levels(pdat2$simple_genre))
  mycolors <- colorRampPalette(bookpal)(nb.cols)
  mycolors[levels(pdat$simple_genre)=='Other'] = 'gray'
  
  rainbowbeauty_mo <- pdat2 %>% 
    ggplot(aes(month_read,total,fill=simple_genre)) + 
    geom_col() +
    scale_fill_manual(values = mycolors) +
    scale_x_continuous(breaks = 1:12,labels = month.abb) +
    xlab('Month') +
    ylab('Total count among books you finished') +
    ggsidekick::theme_sleek(base_size = 14) +
    labs(caption = "Total across all years, genres in color are the long-term most common genres on your shelf.") 
  
  if(whichplot == 'yearly'){
    return(rainbowbeauty)
  }else{
    return(rainbowbeauty_mo)
  }
  
}

#scrape <- scrape_goodreads(npages = 1) #time-consuming
#g <- get_cmatrix_and_genres(scraped_data = scrape)$genres
#bookpal <- c("#ff7506", "#00eaff", "#ffdf06", "#ffee7d", "#ff2673", "#7df4ff") 
#get_rainbow_plot(scraped_data = scrape,pal = bookpal,whichplot = 'yearly')  





library(here)
community <- read.csv(here('output','communitymatrix.csv'),header = T)
cmatrix <- as.matrix(community)
library(vegan)

year <- cmatrix[,1]
divs <- data.frame(year = year,
                   div = diversity(cmatrix[,-1]))

get_divplot <- function(community){
  cmatrix <- as.matrix(community)
  year <- cmatrix[,1]
  divs <- data.frame(year = year,
                     div = diversity(cmatrix[,-1]))
  cmatrix <- as.matrix(community)
  divs %>%
  ggplot(aes(year,div)) +
  geom_point(size=3) +
  geom_line() +
  ylab('Shannon diversity (H)') +
  xlab('Year') +
  labs(#title = 'Genre diversity',
       caption = 'Data: Goodreads') +
  ggsidekick::theme_sleek(base_size = 14)

  }
  
sa <- specaccum(cmatrix[,-1])
plot(sa)

# What I want to do: an NMDS plot of all the books (each point = 1 yr??) showing the topics as arrows on top to show which ones are due to the biggest difference between years
mds <- metaMDS(cmatrix[,-1])
ord <- mds$points %>%
  as.data.frame()

ord %>% 
  add_column(year,group=1) %>%
  filter(!is.na(year)) %>%
  ggplot(aes(MDS1,MDS2,colour = year,group = group)) +
  geom_point(size=3) +
  scale_colour_viridis_c(option='plasma') +
  geom_line() +
  ggpomological::theme_pomological(base_size=16)

data(varespec)
data(varechem)
ord <- metaMDS(varespec)

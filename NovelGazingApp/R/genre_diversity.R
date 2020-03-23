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
  mycolors <- colorRampPalette(pal)(nb.cols)
  mycolors[levels(pdat$simple_genre)=='Other'] = 'gray'
  
  rainbowbeauty <- pdat %>% 
    ggplot(aes(year_read,total,fill=simple_genre)) + 
    geom_col() +
    scale_fill_manual('Your top genres',values = mycolors) +
    xlab('Year') +
    ylab('Count among books you finished') +
    ggsidekick::theme_sleek(base_size = 14) +
    labs(caption = "*Note: Most books have cross-referenced,\n so the genre total will be higher than the number of books you read.") +
    theme(legend.position = 'none')
  
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
  mycolors <- colorRampPalette(pal)(nb.cols)
  mycolors[levels(pdat$simple_genre)=='Other'] = 'gray'
  
  rainbowbeauty_mo <- pdat2 %>% 
    ggplot(aes(month_read,total,fill=simple_genre)) + 
    geom_col() +
    scale_fill_manual('Your top genres',values = mycolors) +
    scale_x_continuous(breaks = 1:12,labels = month.abb) +
    xlab('Month') +
    ylab('Count among books you finished \n (across all years)') +
    ggsidekick::theme_sleek(base_size = 14)# +
    #labs(caption = 'Total across all years') 
  
  if(whichplot == 'yearly'){
    return(rainbowbeauty)
  }else{
    return(rainbowbeauty_mo)
  }
}

#scrape <- scrape_goodreads(npages = 1) #time-consuming
#g <- get_cmatrix_and_genres(scraped_data = goodreads)$genres
#bookpal <- c("#ff7506", "#00eaff", "#ffdf06", "#ffee7d", "#ff2673", "#7df4ff") 
#get_rainbow_plot(scraped_data = goodreads,pal = bookpal,whichplot = 'yearly')  


#library(here)
#community <- read.csv(here('output','communitymatrix.csv'),header = T)
#cmatrix <- as.matrix(community)
#library(vegan)

#year <- cmatrix[,1]
#divs <- data.frame(year = year,
#                   div = diversity(cmatrix[,-1]))

get_divplot <- function(community){
  cmatrix <- as.matrix(community)
  year <- cmatrix[,1]
  divs <- data.frame(year = year,
                     div = diversity(cmatrix[,-1]))
  cmatrix <- as.matrix(community)
  divplot <- divs %>%
    ggplot(aes(year,div)) +
    geom_point(size=3) +
    geom_line() +
    ylab('Shannon diversity (H)') +
    xlab('Year') +
    labs(caption = 'Data: Goodreads') +
    ggsidekick::theme_sleek(base_size = 14)
  return(divplot)
}

#comm <- get_cmatrix_and_genres(scraped_data = goodreads)$community
#get_divplot(community = comm)

get_rarefaction <- function(community,pal){  
  cmatrix <- as.matrix(community)
  year <- cmatrix[,1]
  sa <- specaccum(cmatrix[,-1])
  my.res <- with(sa, data.frame(sites, richness, sd)) %>%
    mutate(year = cmatrix[,1]) %>%
    mutate(loSD = richness - sd,
           hiSD = richness + sd) 
  head(my.res)
  
  plt <- my.res %>% 
    ggplot(aes(year,richness)) +
    geom_ribbon(aes(ymin=loSD,ymax=hiSD),alpha=0.6, fill = pal[4]) +
    geom_line(colour = pal[3],lwd=1.2) +
    scale_x_continuous() +
    ylab('Genre richness') +
    xlab('Year') +
    ggsidekick::theme_sleek(base_size=14) +
    labs(caption = 'Richness +/- 1 SD')
  return(plt)
}

#get_rarefaction(community = comm,pal = bookpal)

# What I want to do: an NMDS plot of all the books. But I have nixed it because NMDS is hard to interpret and is randomized so will look different every time (thus, hard to to interpret!)
get_mds <- function(community,pal){
  cmatrix <-  as.matrix(community)
  year <- cmatrix[,1]
  mds.log <- log(cmatrix[,-1]+1)
  sol <- metaMDS(mds.log)
  vec.sp <- envfit(sol$points, mds.log, perm=1000)
  vec.sp.df <- as.data.frame(vec.sp$vectors$arrows*sqrt(vec.sp$vectors$r))
  vec.sp.df$species <- rownames(vec.sp.df)
  vec.sp.df.tp <- vec.sp.df
  ord <- mds$points %>%
    as.data.frame() %>%
    add_column(year,group=1) %>%
    mutate(year = as.integer(year)) 
  
  ordplot <- 
    ord %>% filter(!is.na(year)) %>%
    ggplot(aes(MDS1,MDS2,color = year,group = factor(group))) +
    geom_point(size=4) +
    geom_path() +
    scale_colour_gradient(low = pal[2],high = pal[3])+
    geom_segment(data=vec.sp.df.tp,
                 aes(x=0,xend=MDS1,y=0,yend=MDS2),
                 inherit.aes = FALSE,
                 arrow = arrow(length = unit(0.5, "cm")),
                 colour="grey") + 
    geom_text(data=vec.sp.df.tp,
              aes(x=MDS1,y=MDS2,label=species),
              inherit.aes = FALSE,size=5) +
    ggsidekick::theme_sleek(base_size=14)
  #ordplot
  
  return(ordplot)
}


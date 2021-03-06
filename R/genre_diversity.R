# genre diversity over time
  
get_rainbow_plot <- function(comm_genre,pal,whichplot){
  #comm_genre is the result of get_cmatrix_and_genres(fulldata = full_data)
  #whichplot is 'yearly' or 'monthly'

  #cleanup genres
  g <- comm_genre$genres
  g <- g %>%
    filter(!is.na(book_genres)) %>%
    filter(!is.na(year_read)) %>%
    filter(textcat::textcat(book_genres)=='english') %>%
    mutate(book_genres = sub("\\ /.*", "", book_genres)) %>%
    mutate(book_genres = sub("\\ --.*", "", book_genres)) %>%
    mutate(book_genres = stringr::str_to_sentence(book_genres)) %>%
    mutate(book_genres = trimws(book_genres))
  # for nested topics, take the largest one; change everything to the same case. Then change to sentence case and trim white space
  
  allgenres <- g %>%
    group_by(year_read,book_genres) %>%
    summarize(total = length(book_genres)) %>%
    ungroup(year_read,book_genres) %>%
    #filter(total>1) %>% 
    mutate(year_read = as.integer(year_read))
  
  topgenres <- g %>%
    group_by(book_genres) %>%
    summarize(bcount = length(book_genres)) %>%
    arrange(desc(bcount)) %>%
    top_n(bcount, n = 15)
  
  topgenres <- topgenres$book_genres
  
  pdat <- allgenres %>%
    mutate(simple_genre = 
             ifelse(book_genres %in% topgenres,
                    book_genres,
                    'Other')) %>%
    group_by(year_read,simple_genre) %>%
    ungroup(simple_genre) %>%
    mutate(simple_genre = fct_relevel(simple_genre, 'Other', after = Inf))
  
  nb.cols <- length(levels(pdat$simple_genre))
  mycolors <- colorRampPalette(pal)(nb.cols)
  mycolors[levels(pdat$simple_genre)=='Other'] = 'gray'
  
  bbreaks <- seq(min(pdat$year_read,na.rm=T),max(pdat$year_read,na.rm=T),1)
  
  
  rb <- pdat %>% 
    ggplot(aes(year_read,total,fill=simple_genre)) + 
    geom_col() +
    scale_x_continuous(labels = scaleFUN) +
    scale_fill_manual('Your top genres',values = mycolors) +
    xlab('Year') +
    ylab('Count among books you finished') +
    ggsidekick::theme_sleek(base_size = 14) +
    theme(legend.position = 'none')
  
  gm <- comm_genre$genres_month
  
  allgenres_month <- gm %>%
    group_by(month_read,book_genres) %>%
    summarize(total = length(book_genres))  %>%
    ungroup(year_read,book_genres) %>%
    mutate(month_read = as.integer(month_read))
  
  pdatm <- allgenres_month %>%
    mutate(simple_genre = ifelse(book_genres %in% topgenres,
                                 book_genres,
                                 'Other')) %>%
    group_by(month_read,simple_genre) %>%
    ungroup(simple_genre) %>%
    mutate(simple_genre = fct_relevel(simple_genre, 'Other', after = Inf))
  
  nb.cols <- length(levels(pdatm$simple_genre))
  mycolors <- colorRampPalette(pal)(nb.cols)
  mycolors[levels(pdatm$simple_genre)=='Other'] = 'gray'
 
  rb_mo <- pdatm %>% 
    ggplot(aes(month_read,total,fill=simple_genre)) + 
    geom_col() +
    scale_fill_manual('Your top topics',values = mycolors) +
    scale_x_continuous(breaks = 1:12,labels = month.abb) +
    xlab('Month') +
    ylab('Count among books you finished \n (across all years)') +
    labs(caption = 'Topic data are from the OpenLibrary API and are not available for all books.') +
    ggsidekick::theme_sleek(base_size = 14) +
    theme(legend.position = 'bottom')
  
  if(whichplot == 'yearly'){return(rb)}else{return(rb_mo)}
}

get_divplot <- function(community){
  cmatrix <- as.matrix(community)
  year <- cmatrix[,1]
  divs <- data.frame(year = year,
                     div = diversity(cmatrix[,-1]))
  #cmatrix <- as.matrix(community)
  bbreaks <- seq(min(divs$year,na.rm=T),
                 max(divs$year,na.rm=T),1)
  divplot <- divs %>%
    ggplot(aes(year,div)) +
    geom_point(size=3) +
    geom_line() +
    scale_x_continuous(breaks = bbreaks) +
    ylab('Shannon diversity (H)') +
    xlab('Year') +
    labs(caption = 'Data: Goodreads') +
    ggsidekick::theme_sleek(base_size = 14)
  return(divplot)
}


get_rarefaction <- function(community,pal){  
  cmatrix <- as.matrix(community)
  year <- cmatrix[,1]
  sa <- specaccum(cmatrix[,-1])
  my.res <- with(sa, data.frame(sites, richness, sd)) %>%
    mutate(year = year) %>%
    mutate(loSD = richness - sd,
           hiSD = richness + sd) 
  bbreaks <- seq(min(my.res$year,na.rm=T),max(my.res$year,na.rm=T),1)
  
  plt <- my.res %>% 
    ggplot(aes(year,richness)) +
    geom_ribbon(aes(ymin=loSD,ymax=hiSD),alpha=0.6, fill = pal[4]) +
    geom_line(colour = pal[3],lwd=1.2) +
    scale_x_continuous(breaks = bbreaks) +
    ylab('Subject richness') +
    xlab('Year') +
    ggsidekick::theme_sleek(base_size=14) +
    labs(caption = 'Richness +/- 1 SD')
  return(plt)
}

#get_rarefaction(community = comm,pal = bookpal)

# What I want to do: an NMDS plot of all the books. But I am so-so on it because NMDS is hard to interpret and is randomized so will look different every time (thus, hard to to interpret!)
get_mds <- function(community,pal){
  cmatrix <-  as.matrix(community)
  year <- cmatrix[,1]
  bbreaks <- seq(min(year,na.rm=T),max(year,na.rm=T),1)
  mds.log <- log(cmatrix[,-1]+1)
  sol <- metaMDS(mds.log)
  vec.sp <- envfit(sol$points, mds.log, perm=500)
  vec.sp.df <- as.data.frame(vec.sp$vectors$arrows*sqrt(vec.sp$vectors$r))
  vec.sp.df$species <- rownames(vec.sp.df)
  vec.sp.df.tp <- vec.sp.df
  ord <- sol$points %>%
    as.data.frame() %>%
    add_column(year,group=1) %>%
    mutate(year = as.integer(year)) 
  
  ordplot <- 
    ord %>% filter(!is.na(year)) %>%
    ggplot(aes(MDS1,MDS2,
               color = year,
               group = factor(group))) +
    geom_point(size=4) +
    geom_path() +
    scale_colour_gradient(low = pal[2],
                          high = pal[3]) +
    ggsidekick::theme_sleek(base_size=14)
  ordplot
  return(ordplot)
}


# MDS comparison ----------------------------------------------------------
# g <- get_cmatrix_and_genres(fulldata = full_data)
# community <- g$community
# community_toread <- g$community_toread

get_mds_comp <- function(community,community_toread,pal){
  if(nrow(community)<2 | nrow(community_toread)<2){
    return(ggplot() + theme_void())} else{
  
  #community
  cmatrix <-  as.matrix(community)
  year <- cmatrix[,1]
  mds.log <- log(cmatrix[,-1]+1)
  sol <- metaMDS(mds.log)
  vec.sp <- envfit(sol$points, mds.log, perm=1000)
  vec.sp.df <- as.data.frame(vec.sp$vectors$arrows*sqrt(vec.sp$vectors$r))
  vec.sp.df$species <- rownames(vec.sp.df)
  vec.sp.df.tp <- vec.sp.df
  ord <- sol$points %>%
    as.data.frame() %>%
    add_column(year,group=1) %>%
    mutate(year = as.integer(year)) %>%
    mutate(Shelf = 'Read')
  
  cmatrix <-  as.matrix(community_toread)
  year <- cmatrix[,1]
  mds.log <- log(cmatrix[,-1]+1)
  sol <- metaMDS(mds.log)
  vec.sp <- envfit(sol$points, mds.log, perm=1000)
  vec.sp.df <- as.data.frame(vec.sp$vectors$arrows*sqrt(vec.sp$vectors$r))
  vec.sp.df$species <- rownames(vec.sp.df)
  vec.sp.df.tp <- vec.sp.df
  ord2 <- sol$points %>%
    as.data.frame() %>%
    add_column(year,group=1) %>%
    mutate(year = as.integer(year))  %>%
    mutate(Shelf = 'To Read')
  
  bigord <- bind_rows(ord,ord2)
  
  ordplot <- 
    bigord %>% 
    filter(!is.na(year)) %>%
    ggplot(aes(MDS1,MDS2,color = Shelf)) +
    geom_point(size=4) +
    scale_color_manual(values = pal[c(3,6)]) +
    ggsidekick::theme_sleek(base_size=16)
  
  ordplot
  
  return(ordplot)}
}

#get_mds_comp(community = community,community_toread = community_toread,pal=bookpal)

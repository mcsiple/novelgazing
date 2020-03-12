# genre diversity over time
library(here)
community <- read.csv(here('output','communitymatrix.csv'),header = T)
cmatrix <- as.matrix(community)
library(vegan)

year <- cmatrix[,1]
divs <- data.frame(year = year,
                   div = diversity(cmatrix[,-1]))


divs %>%
  ggplot(aes(year,div)) +
  geom_point(size=3) +
  geom_line() +
  ylab('Shannon diversity (H)') +
  xlab('Year') +
  labs(title = 'Genre diversity') +
  ggpomological::theme_pomological(base_size=16)

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

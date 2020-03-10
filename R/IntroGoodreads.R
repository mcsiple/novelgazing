

# devtools::install_github("gadenbuie/ggpomological")


library(rgoodreads)
library(httr)  
library(tidyverse)
library(hrbrthemes)
library(gcookbook)
library(lubridate)
library(ggpomological)
library(reshape2)

# Load your own user data separately

read_books %>%
  ggplot(aes(year_read)) +
  geom_bar(stat = "count") +
  labs(title = "Books read per year", 
       caption = "source: Megsie's goodreads") +
  theme_ipsum() +
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
  scale_x_continuous(breaks=seq(2009,2020,by=2),labels=seq(2009,2020,by=2)) +
  ggtitle("Has Megsie become a more critical reader?")


criticplot + theme_pomological("Homemade Apple", 16)

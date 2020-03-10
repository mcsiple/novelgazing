# genre diversity over time
community <- read.csv('communitymatrix.csv',header = T)



cmatrix <- as.matrix(community)
library(vegan)
div <- diversity(cmatrix[,-1])
plot(cmatrix[,1],div)

sa <- specaccum(cmatrix[,-1])
plot(sa)

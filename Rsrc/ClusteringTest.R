library(tidyverse)
library(ggplot2)
library(NbClust)
library(reshape)

ch0 <- read.delim((paste(getwd(),'/','n2','_profil_ch0_prf.txt',sep='')), header=TRUE)
ch0 <- ch0[,1:(which(is.na(ch0[1,]))-1)] #removing last column because it includes NAs that trigger an error with the cluster func
datafr <- t(ch0)
datafr_dist <- dist(datafr, method="euclidean")  
fit <- hclust(datafr_dist, method="ward.D")  
cn <- NbClust(data = datafr, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10,method = "ward.D", index = "ball") #this function finds the best estimated k value, however the index only works with a couple of variables instead of 'all', from what I found it seems like the matrix has negative eigenvalues which the function cant take
plot(fit, family="Arial")
rect.hclust(fit, k = as.numeric(cn$Best.nc[1]), border="cadetblue")
clustered_data <- cutree(fit, k = as.numeric(cn$Best.nc[1]))
clustered_data_tidy <- as.data.frame(as.table(clustered_data)) %>% glimpse()
colnames(clustered_data_tidy) <- c("ID","cluster")
clustered_data_tidy$ID <- as.character(clustered_data_tidy$ID)
table(clustered_data_tidy$cluster)
cluster_IDs <- clustered_data_tidy %>%
  filter(cluster == 1) # cluster we want to pick 

cluster_IDs <- cluster_IDs['ID']
maxrow <- 0
rowmean <- 0
for (k in 1:dim(cluster_IDs)[1]) { #finding the max length for this cluster
  temp2 <- as.numeric(which.min(datafr[k,]))
  rowmean <- (temp2 + rowmean)/2 #gives us average length of each cluster 
  if (maxrow > temp2) {
  } else {
    maxrow <- temp2 
  }
  
}
 for (m in 1:dim(cluster_IDs)[1]) {
          ID <- toString(cluster_IDs[m,1])
          if (m == 1) {
            test <- data.frame(placeholder = as.numeric(datafr[cluster_IDs[m,1],1:maxrow]))
            names(test)[m] <- ID
          } else {
            test$placeholder <- as.numeric(datafr[cluster_IDs[m,1],1:maxrow])
            names(test)[m] <- ID
          }
      }
clustermean <- data.frame(Length = 1:maxrow, Mean = rowMeans(test))
test$Length <- 1:maxrow

newdatafr <- melt(test, id.vars = "Length", measure.vars = c(cluster_IDs[1:50,])) #plotting the first 50 worms
ggplot(newdatafr, aes(Length, value,)) +
  geom_line(color="grey") +
  geom_line(aes(x=Length, y=Mean),clustermean, color = 'red') + #the mean will be the red line 
  theme_minimal() +
  ylab("amp") + xlab("") +
  facet_wrap(~variable)

library(tidyverse) #I'm not sure loading the whole tidyverse package is indeed needed
library(ggplot2)
library(NbClust)
library(reshape)
library(TSclust)

ch1 <- read.delim((paste(getwd(),'/','n2','_profil_ch1_prf.txt',sep='')), header=TRUE) 
ch1 = ch1[,-c(which(is.na(ch1[1,])))]
datafr <- t(ch1)
#Creating the labels for each worm 
TOF <- matrix(,nrow=length(datafr[,1]),ncol = 2 )

for (x in 1:length(datafr[,1])) {
  TOF[x,1] <- which.min(datafr[x,])
  if (as.numeric(TOF[x,1]) < 60 ) {TOF[x,2] <- "NA"} # I wasn't sure what to call things less than L1
  else if (as.numeric(TOF[x,1]) >= 60 & as.numeric(TOF[x,1]) < 90 ) {TOF[x,2] <- "L1"} 
  else if (as.numeric(TOF[x,1]) >= 90 & as.numeric(TOF[x,1]) < 200 ) {TOF[x,2] <- "L2/L3"} 
  else if (as.numeric(TOF[x,1]) >= 200 & as.numeric(TOF[x,1]) < 300 ) {TOF[x,2] <- "L4"} 
  else if (as.numeric(TOF[x,1]) >= 300) {TOF[x,2] <- "Adult"}
}
#index number for each stage 
adultI <- which(TOF[,2]=='Adult')
L1I <- which(TOF[,2]=='L1')
L23I <- which(TOF[,2]=='L2/L3')
L4I <- which(TOF[,2]=='L4')

#data frames for each stage 
datafrAdult <- datafr[adultI,]
datafrL1 <- datafr[L1I,]
datafrL23 <- datafr[L23I,]
datafrL4 <- datafr[L4I,]

datafr_distAdult <- dist(datafrAdult, method ="euclidean")
fit <- hclust(datafr_distAdult, method="ward.D")  
cn <- NbClust(data = datafrAdult, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10,method = "ward.D", index = "ball") 
clustered_data <- cutree(fit, k = as.numeric(cn$Best.nc[1]))
clustered_data_tidy <- as.data.frame(as.table(clustered_data)) %>% glimpse()
colnames(clustered_data_tidy) <- c("ID","cluster")
clustered_data_tidy$ID <- as.character(clustered_data_tidy$ID)
table(clustered_data_tidy$cluster)

###
for(cl_id in as.integer(names(table(clustered_data_tidy$cluster)))){
  
  
  cluster_IDs <- clustered_data_tidy %>%
    filter(cluster == cl_id)
  
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
  
  
  p = ggplot(newdatafr, aes(Length, value,)) +
    geom_line(color="grey") +
    geom_line(aes(x=Length, y=Mean),clustermean, color = 'red') + #the mean will be the red line 
    theme_minimal() +
    ylab("amp") + xlab("") +
    facet_wrap(~variable)
  
  
  ggsave(paste("Cluster_",cl_id,"_first50Worms.pdf",sep=""),p)
  
}

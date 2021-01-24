library(tidyverse) #I'm not sure loading the whole tidyverse package is indeed needed 
library(ggplot2)
library(NbClust)
library(reshape)
library(TSclust)

ch0 <- read.delim((paste(getwd(),'/','n2','_profil_ch0_prf.txt',sep='')), header=TRUE) 
ch1 <- read.delim((paste(getwd(),'/','n2','_profil_ch1_prf.txt',sep='')), header=TRUE) 
ch2 <- read.delim((paste(getwd(),'/','n2','_profil_ch2_prf.txt',sep='')), header=TRUE) 
ch3 <- read.delim((paste(getwd(),'/','n2','_profil_ch3_prf.txt',sep='')), header=TRUE) 
###AMV:Working from root directory, i.e. *git_directory*/WormSorter/, does not load properly. The path I used to make it work was: *git_directory*/WormSorter/data/N2 
ch0 = ch0[,-c(which(is.na(ch0[1,])))]
ch1 = ch1[,-c(which(is.na(ch1[1,])))]
ch2 = ch2[,-c(which(is.na(ch2[1,])))]
ch3 = ch3[,-c(which(is.na(ch3[1,])))]
datafrch0 <- t(ch0)
datafrch1 <- t(ch1)
datafrch2 <- t(ch2)
datafrch3 <- t(ch3)
#Removing worms with a high ch0 TOF amplitude 
WormNumber <- which(datafrch0 > 64000, arr.ind = TRUE) 
WormNumber <- unique(WormNumber[,1]) 
WormNumber <- sort(WormNumber)
#Removing the worms we filtered out from the data frame
datafrch0 <- datafrch0[-c(WormNumber),]
datafrch1 <- datafrch1[-c(WormNumber),]
datafrch2 <- datafrch2[-c(WormNumber),]
datafrch3 <- datafrch3[-c(WormNumber),]
#Creating the labels for each worm 
TOF <- matrix(,nrow=length(datafrch0[,1]),ncol = 2 )

for (x in 1:length(datafrch0[,1])) {
  TOF[x,1] <- which.min(datafrch0[x,])
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
#ch1 
datafrch1L1 <- datafrch1[L1I,] 
datafrch1L23 <- datafrch1[L23I,]
datafrch1L4 <- datafrch1[L4I,]
datafrch1Adult <- datafrch1[adultI,]
#ch2
datafrch2Adult <- datafrch2[adultI,]
datafrch2L1 <- datafrch2[L1I,]
datafrch2L23 <- datafrch2[L23I,]
datafrch2L4 <- datafrch2[L4I,]
#ch3
datafrch3Adult <- datafrch3[adultI,]
datafrch3L1 <- datafrch3[L1I,]
datafrch3L23 <- datafrch3[L23I,]
datafrch3L4 <- datafrch3[L4I,]

#Running the clustering for each stage in each channel 

for(c in 1:3) {
  for (j in 1:4) {
    if (j == 1) {
      channelstage <- paste('datafrch',c,'Adult',sep='')
    } else if (j == 2) {  channelstage <- paste('datafrch',c,'L1',sep='') }
    else if (j == 3) {channelstage <- paste('datafrch',c,'L23',sep='')}
    else if (j == 4) {channelstage <- paste('datafrch',c,'L4',sep='')}
    
    datafr_dist <- dist(eval(as.name(channelstage)), method ="euclidean")
    fit <- hclust(datafr_dist, method="ward.D")
    #cn <- NbClust(data = datafrAdult, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10,method = "ward.D", index = "ball") 
    clustered_data <- cutree(fit, k = 3 ) #For now i picked two clusters, to use NbClust we replace it with -> as.numeric(cn$Best.nc[1])
    clustered_data_tidy <- as.data.frame(as.table(clustered_data)) 
    colnames(clustered_data_tidy) <- c("ID","cluster")
    clustered_data_tidy$ID <- as.character(clustered_data_tidy$ID)
    table(clustered_data_tidy$cluster)
    for(cl_id in as.integer(names(table(clustered_data_tidy$cluster)))){
      
      
      cluster_IDs <- clustered_data_tidy %>%
        filter(cluster == cl_id)
      
      cluster_IDs <- cluster_IDs['ID']
      maxrow <- 0
      rowmean <- 0
      for (k in 1:dim(cluster_IDs)[1]) { #finding the max length for this cluster
        temp2 <- as.numeric(which.min(eval(as.name(channelstage))[k,]))
        rowmean <- (temp2 + rowmean)/2 #gives us average length of each cluster 
        if (maxrow > temp2) {
        } else {
          maxrow <- temp2 
        }
        
      }
      for (m in 1:dim(cluster_IDs)[1]) {
        ID <- toString(cluster_IDs[m,1])
        if (m == 1) {
          test <- data.frame(placeholder = as.numeric(eval(as.name(channelstage))[cluster_IDs[m,1],1:maxrow]))
          names(test)[m] <- ID
        } else {
          test$placeholder <- as.numeric(eval(as.name(channelstage))[cluster_IDs[m,1],1:maxrow])
          names(test)[m] <- ID
        }
      }
      clustermean <- data.frame(Length = 1:maxrow, Mean = rowMeans(test))
      test$Length <- 1:maxrow
      if(nrow(cluster_IDs) > 50){maxL = 50}else{maxL = nrow(cluster_IDs)}
      newdatafr <- melt(test, id.vars = "Length", measure.vars = c(cluster_IDs[1:maxL,])) 
      
      
      p = ggplot(newdatafr, aes(Length, value,)) +
        geom_line(color="grey") +
        geom_line(aes(x=Length, y=Mean),clustermean, color = 'red') + #the mean will be the red line 
        theme_minimal() +
        ylab("amp") + xlab("") +
        facet_wrap(~variable)
      
      
      ggsave(paste("Cluster_",cl_id,"_first50Worms",channelstage,".pdf",sep=""),p)
      
    }
    
  }
  
}
###

PlotClusters <- function(Channel,ClusterTable) {
  if(missing(Channel)){ 
    stop('Channel dataframe was not provided')
  } else if(missing(ClusterTable)) {
    stop('Cluster list was not provided')
  } else if(!list(ClusterTable)) {
    stop('Cluster list provided is not a list')
  } else if(!frame(Channel)) {
    stop('Channel dataframe provided is not a dataframe')
  }
  Stage <- c('L1','L23','L4','Adult')
  for (i in 1:4) {
    for(cl_id in as.integer(names(table(ClusterTable[[i]]$cluster)))) {
      
    cluster_IDs <- ClusterTable[[i]] %>%
      filter(cluster == cl_id)
    cluster_IDs <- cluster_IDs['ID']
    maxrow <- 0
    rowmean <- 0
    for (k in 1:dim(cluster_IDs)[1]) { #finding the max length for this cluster
      temp2 <- as.numeric(which.min(Channel[k,]))
      rowmean <- (temp2 + rowmean)/2 #gives us average length of each cluster 
      if (maxrow > temp2) {
      } else {
        maxrow <- temp2 
      }
      
    }
    for (m in 1:dim(cluster_IDs)[1]) {
      ID <- toString(cluster_IDs[m,1])
      if (m == 1) {
        test <- data.frame(placeholder = as.numeric(Channel[cluster_IDs[m,1],1:maxrow]))
        names(test)[m] <- ID
      } else {
        test$placeholder <- as.numeric(Channel[cluster_IDs[m,1],1:maxrow])
        names(test)[m] <- ID
      }
    }
    clustermean <- data.frame(Length = 1:maxrow, Mean = rowMeans(test))
    test$Length <- 1:maxrow
    if(nrow(cluster_IDs) > 50){maxL = 50}else{maxL = nrow(cluster_IDs)}
    newdatafr <- melt(test, id.vars = "Length", measure.vars = c(cluster_IDs[1:maxL,])) 
    
    
    p = ggplot(newdatafr, aes(Length, value,col = variable)) +
      geom_line(color="grey") +
      geom_line(aes(x=Length, y=Mean),clustermean, color = 'red') + #the mean will be the red line 
      theme_minimal() +
      ylab("amp") + xlab("") 
    
    
    ggsave(paste("Cluster_",cl_id,Stage[i],".pdf",sep=""),p)
    }
  }
  
}
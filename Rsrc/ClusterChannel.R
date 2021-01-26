ClusterChannel <- function(Channel,m) {
  #returns a list with 4 dataframes, each dataframe represents a stage and its corresponding clusters 
  library(TSclust)
  if(missing(Channel)){ 
    stop('Channel dataframe was not provided')
  } else if(missing(m)) {
    stop('Number of clusters was not provided')
  } 
  TOF <- matrix(,nrow=length(Channel[,1]),ncol = 2 )
  
  for (x in 1:length(Channel[,1])) {
    TOF[x,1] <- which.min(Channel[x,])
    if (as.numeric(TOF[x,1]) < 60 ) {TOF[x,2] <- "NA"} # I wasn't sure what to call things less than L1
    else if (as.numeric(TOF[x,1]) >= 60 & as.numeric(TOF[x,1]) < 90 ) {TOF[x,2] <- "L1"} 
    else if (as.numeric(TOF[x,1]) >= 90 & as.numeric(TOF[x,1]) < 200 ) {TOF[x,2] <- "L2/L3"} 
    else if (as.numeric(TOF[x,1]) >= 200 & as.numeric(TOF[x,1]) < 300 ) {TOF[x,2] <- "L4"} 
    else if (as.numeric(TOF[x,1]) >= 300) {TOF[x,2] <- "Adult"}
  }
  L1 <- Channel[which(TOF[,2]=='L1'),] 
  L23 <- Channel[which(TOF[,2]=='L2/L3'),]
  L4 <- Channel[which(TOF[,2]=='L4'),]
  Adult <- Channel[which(TOF[,2]=='Adult'),]
  ChannelStage <- c('L1','L23','L4','Adult')
  ClusterList <- vector("list", 4)
  i <- 1
  for (j in ChannelStage) {
    datafr_dist <- dist(eval(as.name(j)), method ="euclidean")
    fit <- hclust(datafr_dist, method="ward.D")
    clustered_data <- cutree(fit, k = m ) 
    clustered_data_tidy <- as.data.frame(as.table(clustered_data)) 
    colnames(clustered_data_tidy) <- c("ID","cluster")
    clustered_data_tidy$ID <- as.character(clustered_data_tidy$ID)
    ClusterList[[i]] <- clustered_data_tidy
    i <- i + 1
  }
  return(ClusterList)
}
CreateTrainingSetIDs <- function(StageChannelData, IndexDirectory) {
  #create if statements to make sure everything is correct type and stuff
  
  ##1 is good and 2 is bad 
  GoodIDs <- read.delim(IndexDirectory, sep=',',header=TRUE) 
  GoodWormIndex <- which(GoodIDs[2,] == 'Good')
  BadWormIndex <- which(GoodIDs[2,] == 'Bad')
  GoodWormIndex <- unname(GoodIDs[1,GoodWormIndex])
  BadWormIndex <- unname(GoodIDs[1,BadWormIndex])
  #GoodIndex <- matrix(GoodIndex, ncol = ncol(GoodIndex), dimnames = NULL)
  #For loop that randomly picks a specific number of good worms (from the training set) to try and predict the remaining worms
  Output <- list()
  Output[[1]] <- matrix(GoodWormIndex[which(GoodWormIndex[1,] %in% rownames(StageChannelData))]) 
  Output[[2]] <- matrix(BadWormIndex[which(BadWormIndex[1,] %in% rownames(StageChannelData))]) 
  #Output[[1]] <- GoodWormIndex
  #Output[[2]] <- BadWormIndex
  return(Output)
  
}









 



























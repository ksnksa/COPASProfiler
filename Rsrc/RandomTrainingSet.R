#create randomized training set and predection set based on your parameters 

RandomTrainingSet<- function(StageChannelData,WormIDs, SizeGood, SizeBad) {
  #create if statements to make sure everything is correct type and stuff
  BadWorms <- StageChannelData[which(rownames(StageChannelData) %in% WormIDs[[2]]),]
  #BadWorms <- BadWorms[1:which(rownames(BadWorms) == WormIDs[[2]][dim(WormIDs[[2]])[1],]),]
  
  
  
  RandomIndex <- sample(c(1:length(WormIDs[[1]])), size=SizeGood, replace = FALSE)
  RandomIndex <- sort(RandomIndex)
  ComplementVec <- 1:length(WormIDs[[1]])
  ComplementVec <- ComplementVec[-RandomIndex]
  RandomIndex <- WormIDs[[1]][RandomIndex]
  PredIndex <- as.matrix(WormIDs[[1]][ComplementVec])
  IndexMax <- max( as.numeric(sub('X','',RandomIndex)))
  PredBadWormsIndex <- sample(c(1:nrow(WormIDs[[2]])),size = SizeBad, replace = FALSE)
  PredBadWormsIndex <- sort(PredBadWormsIndex)
  PredictionSet <- as.data.frame(rbind(BadWorms[-PredBadWormsIndex,],Adult[as.numeric(as.numeric(which(rownames(Adult) %in% PredIndex))),]))
  
  
  TrainingSet <- as.data.frame(rbind(BadWorms[PredBadWormsIndex,],Adult[as.numeric(which(rownames(Adult) %in% RandomIndex)),]))
  TrainingSet$Factor <- as.numeric(1) #creating a true false table
  TrainingSet$Factor[(dim(BadWorms[PredBadWormsIndex,])[1]+1):nrow(TrainingSet)] <- as.numeric(2) 
  Output <- list()
  Output[[1]] <- TrainingSet
  Output[[2]] <- PredictionSet
  return(Output)
  
}


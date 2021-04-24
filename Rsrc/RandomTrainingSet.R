#create randomized training set and predection set based on your parameters 

RandomTrainingSet<- function(StageChannelData,WormIDs, SizeGood, SizeBad) {

  if(missing(StageChannelData)){ 
    stop('Missing StageChannelData input')
  } else if(missing(WormIDs)) {
    stop('Missing WormIDs input')
  }else if(missing(SizeGood)) {
    stop('Missing SizeGood input')
  }else if(missing(SizeBad)) {
    stop('Missing SizeBad input')
  } else if(typeof(StageChannelData) != 'list') {
    stop('StageChannelData type is expected to be list, please provide the correct input type') 
  } else if(typeof(WormIDs) != 'list') {
    stop('WormIDs type is expected to be list, please provide the correct input type') 
  } else if(typeof(SizeGood) != 'double') {
    stop('SizeGood type is expected to be double, please provide the correct input type') 
  } else if(typeof(SizeBad) != 'double') {
    stop('SizeBad type is expected to be double, please provide the correct input type') 
  }
  
  
  
  BadWorms <- StageChannelData[which(rownames(StageChannelData) %in% WormIDs[[2]]),]
  RandomIndex <- sample(c(1:length(WormIDs[[1]])), size=SizeGood, replace = FALSE)
  RandomIndex <- sort(RandomIndex)
  ComplementVec <- 1:length(WormIDs[[1]])
  ComplementVec <- ComplementVec[-RandomIndex]
  RandomIndex <- WormIDs[[1]][RandomIndex]
  PredIndex <- as.matrix(WormIDs[[1]][ComplementVec])
  IndexMax <- max( as.numeric(sub('X','',RandomIndex)))
  PredBadWormsIndex <- sample(c(1:nrow(WormIDs[[2]])),size = SizeBad, replace = FALSE)
  PredBadWormsIndex <- sort(PredBadWormsIndex)
  PredictionSet <- as.data.frame(rbind(BadWorms[-PredBadWormsIndex,],StageChannelData[as.numeric(as.numeric(which(rownames(StageChannelData) %in% PredIndex))),]))
  
  
  TrainingSet <- as.data.frame(rbind(BadWorms[PredBadWormsIndex,],StageChannelData[as.numeric(which(rownames(StageChannelData) %in% RandomIndex)),]))
  TrainingSet$Factor <- as.numeric(1) #creating a true false table
  TrainingSet$Factor[(dim(BadWorms[PredBadWormsIndex,])[1]+1):nrow(TrainingSet)] <- as.numeric(2) 
  Output <- list()
  Output[[1]] <- TrainingSet
  Output[[2]] <- PredictionSet
  return(Output)
  
}


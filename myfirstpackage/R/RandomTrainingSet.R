#create randomized training set and prediction set based on your parameters
#' Create random training set and prediction set
#'
#' The function takes in the worm data set and creates a training set and a prediction set based on the input parameters.
#' The parameters are: The category each worm is in, how many good worms and how many bad worms to include in the
#' training set.
#' Each output will provide a different training and prediction set. Consequently, allowing the user to
#' measure the accuracy of the clustering/categorizing algorithm.
#'
#' @param StageChannelData The channel data
#' @param WormIDs File containing the category and worm ID of the channel data.
#' @param SizeGood How many good worms to be included in the training set.
#' @param SizeBad How many bad worms to be included in the training set.
#' @return List containing the training set and the prediction set.
#' @export
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


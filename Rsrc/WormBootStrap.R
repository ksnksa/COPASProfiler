require(e1071)
require(ggplot2)
#require(reshape)
#require(dplyr)

#require(cowplot)



WormBootStrap <- function(Ch0D,Ch1D,Ch2D,Ch3D,MaxTOF, MinLength,MaxLength, GoodIDD, NumberOfRuns, NumberOfBadWorms, NumbersOfGoodWorms, ChannelToCluster, WormStage) {
  if(missing(Ch0D)){ 
    stop('Missing Ch0D input')
  } else if(missing(Ch1D)) {
    stop('Missing Ch1D input')
  }else if(missing(Ch2D)) {
    stop('Missing Ch2D input')
  } else if(missing(Ch3D)){ 
    stop('Missing Ch3D input')
  } else if(missing(MaxTOF)) {
    stop('Missing MaxTOF input')
  }else if(missing(MinLength)) {
    stop('Missing MinLength input')
  } else if(missing(MaxLength)){ 
    stop('Missing MaxLength input')
  } else if(missing(GoodIDD)) {
    stop('Missing GoodIDD input')
  }else if(missing(NumberOfRuns)) {
    stop('Missing NumberOfRuns input')
  } else if(missing(NumberOfBadWorms)){ 
    stop('Missing NumberOfBadWorms input')
  } else if(missing(NumbersOfGoodWorms)) {
    stop('Missing NumbersOfGoodWorms input')
  }else if(missing(ChannelToCluster)) {
    stop('Missing ChannelToCluster input')
  } else if(missing(WormStage)) {
    stop('Missing WormStage input')
  } else if(typeof(Ch0D) != 'character') {
    stop('Ch0D type is expected to be character, please provide the correct input type') 
  } else if(typeof(Ch1D) != 'character') {
    stop('Ch1D type is expected to be character, please provide the correct input type') 
  } else if(typeof(Ch2D) != 'character') {
    stop('Ch2D type is expected to be character, please provide the correct input type') 
  } else if(typeof(Ch3D) != 'character') {
    stop('Ch3D type is expected to be character, please provide the correct input type') 
  } else if(typeof(MaxTOF) != 'double') {
    stop('MaxTOF type is expected to be double, please provide the correct input type') 
  } else if(typeof(MinLength) != 'double') {
    stop('MinLength type is expected to be double, please provide the correct input type') 
  } else if(typeof(MaxLength) != 'double') {
    stop('MaxLength type is expected to be double, please provide the correct input type') 
  } else if(typeof(GoodIDD) != 'character') {
    stop('GoodIDD type is expected to be character, please provide the correct input type') 
  } else if(typeof(NumberOfRuns) != 'double') {
    stop('NumberOfRuns type is expected to be double, please provide the correct input type') 
  } else if(typeof(NumberOfBadWorms) != 'double') {
    stop('NumberOfBadWorms type is expected to be double, please provide the correct input type') 
  } else if(typeof(NumbersOfGoodWorms) != 'double') {
    stop('NumbersOfGoodWorms type is expected to be double, please provide the correct input type') 
  } else if(typeof(ChannelToCluster) != 'double') {
    stop('ChannelToCluster type is expected to be double, please provide the correct input type') 
  } else if(typeof(WormStage) != 'character') {
    stop('WormStage type is expected to be character, please provide the correct input type') 
  }
  
  source((paste(getwd(),'/Rsrc','/','ReadChannel.R',sep='')))
  channellist <- ReadChannel(Ch0D,Ch1D,Ch2D,Ch3D)
  source((paste(getwd(),'/Rsrc','/','FilterChannel.R',sep='')))
  Index <- FilterChannel(channellist[[ChannelToCluster]],MaxTOF,MinLength,MaxLength)
  channellist[[ChannelToCluster]] <- as.data.frame(channellist[[ChannelToCluster]][-c(Index),])
  
  source((paste(getwd(),'/Rsrc','/','AssignStage.R',sep='')))
  StageList <- AssignStage(channellist[[ChannelToCluster]])
  if (WormStage == 'Adult') { 
    WormData <- as.data.frame(StageList[[4]])
  } else if (WormStage == 'L4') {
    WormData<- as.data.frame(StageList[[3]])
  } else if (WormStage == 'L23') {
    WormData <- as.data.frame(StageList[[2]])
  } else if (WormStage == 'L1') {
    WormData <- as.data.frame(StageList[[1]])
  }

  source((paste(getwd(),'/Rsrc','/','CreateTrainingSetIDs.R',sep='')))
  WormIDs <- CreateTrainingSetIDs(WormData,GoodIDD)
  source((paste(getwd(),'/Rsrc','/','RandomTrainingSet.R',sep='')))
  source((paste(getwd(),'/Rsrc','/','GetPrediction.R',sep='')))
  Accuracy <- data.frame()

  for (l in 1:length(NumbersOfGoodWorms)) {
    for (o in 1:NumberOfRuns) {
      SetList<- RandomTrainingSet(WormData,WormIDs,NumbersOfGoodWorms[l],NumberOfBadWorms)
      Pred <- GetPrediction(SetList)
      Positive <- rownames(SetList[[2]][which(Pred==2),])
      TP <- sum((Positive %in% WormIDs[[1]]), na.rm = TRUE)
      Negative <- rownames(SetList[[2]][which(Pred==1),])
      TN <- length(Negative %in% WormIDs[[2]]) - sum((Negative %in% WormIDs[[1]]), na.rm = TRUE)
      Accuracy[l,o] <- (TP + TN) / (length(Positive) + length(Negative))
    }
    
  }
  source((paste(getwd(),'/Rsrc','/','PlotAccuracy.R',sep='')))
  p <- PlotAccuracy(Accuracy, NumbersOfGoodWorms, NumberOfBadWorms)
  
  return(p)
  
}


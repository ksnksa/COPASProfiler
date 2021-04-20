#THIS IS FOR THE BOOTSTRAP THING 
#arguments needed Channels directorys (for now x4), hard filtering using length and max amp (3),
# Good data directory (1), number of runs (1), set number of good and bad worms (for now x3 can be x4)  also which channel to cluster (1), stage you wanna work with (1). 
require(e1071)
require(ggplot2)
require(reshape)
require(dplyr)

require(cowplot)



WormBootStrap <- function(Ch0D,Ch1D,Ch2D,Ch3D,MaxTOF, MinLength,MaxLength, GoodIDD, NumberOfRuns, NumberOfBadWorms, NumbersOfGoodWorms, ChannelToCluster, WormStage) {
  #create if statements to make sure everything is correct type and stuff
  source((paste(getwd(),'/Rsrc','/','ReadChannel.R',sep='')))
  channellist <- ReadChannel(Ch0D,Ch1D,Ch2D,Ch3D)
  #Still using the original filtering before the clustering 
  source((paste(getwd(),'/Rsrc','/','FilterChannel.R',sep='')))
  Index <- FilterChannel(channellist[[ChannelToCluster]],MaxTOF,MinLength,MaxLength)
  channellist[[ChannelToCluster]] <- channellist[[ChannelToCluster]][-c(Index),]
  
  source((paste(getwd(),'/Rsrc','/','AssignStage.R',sep='')))
  StageList <- AssignStage(channellist[[ChannelToCluster]])
  if (WormStage == 'Adult') { 
    Adult <- StageList[[4]]
  } else if (WormStage == 'L4') {
    Adult <- StageList[[3]]
  } else if (WormStage == 'L23') {
    Adult <- StageList[[2]]
  } else if (WormStage == 'L1') {
    Adult <- StageList[[1]]
  }
  

  #L1 <- StageList[[1]]
  #L23 <- StageList[[2]]
  #L4 <- StageList[[3]]
  #Adult <- StageList[[4]]
  source((paste(getwd(),'/Rsrc','/','CreateTrainingSetIDs.R',sep='')))
  AdultIDs <- CreateTrainingSetIDs(Adult,GoodIDD)
  source((paste(getwd(),'/Rsrc','/','RandomTrainingSet.R',sep='')))
  source((paste(getwd(),'/Rsrc','/','GetPrediction.R',sep='')))
  pred <- list()
  Accuracy <- data.frame()

  for (l in 1:length(NumbersOfGoodWorms)) {
    for (o in 1:NumberOfRuns) {
      SetList<- RandomTrainingSet(Adult,AdultIDs,NumbersOfGoodWorms[l],NumberOfBadWorms)
      Pred <- GetPrediction(SetList)
      Positive <- rownames(SetList[[2]][which(Pred==2),])
      TP <- sum((Positive %in% AdultIDs[[1]]), na.rm = TRUE)
      Negative <- rownames(SetList[[2]][which(Pred==1),])
      TN <- length(Negative %in% AdultIDs[[2]]) - sum((Negative %in% AdultIDs[[1]]), na.rm = TRUE)
      Accuracy[l,o] <- (TP + TN) / (length(Positive) + length(Negative))
    }
    
  }
  source((paste(getwd(),'/Rsrc','/','PlotAccuracy.R',sep='')))
  p <- PlotAccuracy(Accuracy, NumbersOfGoodWorms, NumberOfBadWorms)
  
  return(p)
  
}


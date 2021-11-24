#' Classifies input data using the input model
#'
#' The function takes in the input data and runs classification using the input model.
#'
#' @param DataDirectory Directory of the COPAS output file.
#' @param ModelDirectory Directory of the model to be used, ends in .R
#' @param MaxAmp Maximum optical density, anything bigger will be removed.
#' @param MinLength Minimal object length, anything smaller will be removed.
#' @param MaxLength Maximum object length, anything bigger will be removed.
#' @param TypeOfData Type of COPAS output file. Either the first channel (optical density) file or the full unsplit channel file.
#'
#' @return Returns annotated IDs after classification
#' @export
RunClassification <- function(DataDirectory,ModelDirectory,MaxAmp,MinLength,MaxLength,TypeOfData = 'FirstChannel') {

  if (TypeOfData == 'FirstChannel') {
    Ch0 <- read.delim(DataDirectory, header=TRUE)
    Ch0 = t(Ch0[,-c(which(is.na(Ch0[1,])))])
  } else if (TypeOfData == 'FullFile') {
    FullFile <- read.delim(DataDirectory)
    NewCh0 <- FullFile
    for (x in 1:dim(FullFile)[1]) {
      NewCh0[,x] <- na.omit(FullFile[,x])

    }
    x <- NewCh0[,-which(grepl('.', colnames(NewCh0), fixed = TRUE))]
    Ch0 <- t(x)
    Ch0 <- Ch0[-dim(Ch0)[1],]
  }
  Index <- FilterChannel(Ch0,MaxAmp,MinLength,MaxLength)
  channellist <- list()
  ChannelToCluster <- 1
  channellist[[ChannelToCluster]] <- Ch0
  channellist[[ChannelToCluster]] <- as.data.frame(channellist[[ChannelToCluster]][-c(Index),])
  ModifiedData <- matrix(nrow= dim(channellist[[ChannelToCluster]])[1], ncol =100)
  for (z in 1:dim(channellist[[ChannelToCluster]])[1]) {
    if (max(which(channellist[[ChannelToCluster]][z,]!=0)) < 100 ) {
      ModifiedData[z,] <- as.numeric(binning(channellist[[ChannelToCluster]][z,1:100],100))
    } else { ModifiedData[z,] <- as.numeric(binning(channellist[[ChannelToCluster]][z,1:(max(which(channellist[[ChannelToCluster]][z,]!=0)) + 1)],100))
    }
  }
  RowNames <- rownames_to_column(channellist[[ChannelToCluster]],'ID')[,1]
  ModData <- data.frame(ModifiedData,row.names = RowNames)
  for (row in 1:dim(ModData)[1]) {
    ModData[row,] <- rescale(as.numeric(ModData[row,]), to = c(0,1))
  }
  load(ModelDirectory)
  DataSet <- ModData


  PredResults <- predict(Model,DataSet)
  Pos <- rownames(DataSet[which(PredResults==2),])
  Neg <- rownames(DataSet[which(PredResults==1),])
  Output <- list()
  Output[[1]] <- as.character(Pos)
  Output[[2]] <- as.character(Neg)
  return(Output)
}

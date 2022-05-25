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
RunClassification <- function(DataDirectory,ModelDirectory,MaxAmp,MinLength,MaxLength,TypeOfData = 'FirstChannel',ChannelToCluster='1') {

  ModelName <- load(ModelDirectory)
  Model <- get(ModelName)
  ModelVectorLength <- dim(Model@xmatrix[[1]])[2]
  if (ModelVectorLength >= MinLength) {
    stop(paste('Model vector length is:',ModelVectorLength,' Minimum Length input must be at least one more than Model vector length'))
  }
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
  UnModData <- channellist[[ChannelToCluster]]
  ModData3 <- UnModData[,1:(ModelVectorLength)]
  for (row in 1:(dim(UnModData)[1]-1)){
    ModData3[row,] <- paa(as.numeric(UnModData[row,1:((which(as.numeric(UnModData[row,]) == 0)[1] -1))]),ModelVectorLength)
  }
  for (row in 1:dim(ModData3)[1]) {
    ModData3[row,] <- scale(as.numeric(ModData3[row,]))
  }

  DataSet <- ModData3


  PredResults <- predict(Model,DataSet)
  Pos <- rownames(DataSet[which(PredResults==2),])
  Neg <- rownames(DataSet[which(PredResults==1),])
  Output <- list()
  Output[[1]] <- as.character(Pos)
  Output[[2]] <- as.character(Neg)
  return(Output)
}

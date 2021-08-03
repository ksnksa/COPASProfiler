#' Plots the provided worm sorter output files into a summary boxplot
#'
#' The function takes in the data set directories, the names of the strains and plots the desired fluorescence channel.
#' The boxplot provides a summary of the amplitude of the fluorescence of each strain. 
#' 
#' @param FileDirectories Directories to the worm sorter summary output file. In the format of <- c('Directory1','Directory2') and so on.
#' @param Names Names of the strains that will be used in the plots. In the format of <- c('Name1','Name2') and so on.
#' @param FluorescenceChannel Which channel to plot: G is green, R is red, and Y is yellow. 
#' 
#' @return Returns a summary boxplot of the fluoresence. 
#' @export
SummaryPlots <- function (FileDirectories,Names,FluorescenceChannel) { 
  if(missing(FileDirectories)){
    stop('Missing FileDirectories input')
  }  else if(typeof(FileDirectories) != 'character') {
    stop('FileDirectories type is expected to be character, please provide the correct input type')
  } else if(missing(Names)){
    stop('Missing Names input')
  }  else if(typeof(Names) != 'character') {
    stop('Names type is expected to be character, please provide the correct input type')
  } else if(missing(FluorescenceChannel)){
    stop('Missing FluorescenceChannel input')
  }  else if(typeof(FluorescenceChannel) != 'character') {
    stop('FluorescenceChannel type is expected to be character, please provide the correct input type') }
  if(!require("ggplot2")){
    library("ggplot2")
  }
  if(!require("dplyr")){
    library("dplyr")
  }
  if(!require("reshape2")){
    library("reshape2")
  }
  ChannelSummary <- list()
  for (x in 1:length(FileDirectories)) { 
    ChannelSummary[[x]] <- read.delim(FileDirectories[x], header=TRUE)
  }
  DataList <- list()
  for (z in 1:length(ChannelSummary)) { 
    IDTOF <- matrix(nrow=which.max(ChannelSummary[[z]][,1]),ncol=8)
    IDTOF <- data.frame(IDTOF)
    colnames(IDTOF) <- c('ID','TOF','EXT','G','Y','R','PH.EXT','Stage')
    IDTOF[,1] <- ChannelSummary[[z]][1:dim(IDTOF)[1],1]
    IDTOF[,2] <- ChannelSummary[[z]][1:dim(IDTOF)[1],10]
    IDTOF[,3] <- ChannelSummary[[z]][1:dim(IDTOF)[1],11]
    IDTOF[,4] <- ChannelSummary[[z]][1:dim(IDTOF)[1],12]
    IDTOF[,5] <- ChannelSummary[[z]][1:dim(IDTOF)[1],13]
    IDTOF[,6] <- ChannelSummary[[z]][1:dim(IDTOF)[1],14]
    IDTOF[,7] <- ChannelSummary[[z]][1:dim(IDTOF)[1],15]
    Stages <- c('50-75','75-150','150-225','225-500','500-800')
    for (x in 1:dim(IDTOF)[1]) {
      if (as.numeric(IDTOF[x,'TOF']) < 50) {IDTOF[x,'Stage'] <- 'TooSmall'}
      
      else if (as.numeric(IDTOF[x,'TOF']) >= 50 & as.numeric(IDTOF[x,'TOF']) < 75) {IDTOF[x,'Stage'] <- Stages[1]}
      else if (as.numeric(IDTOF[x,'TOF']) >= 75 & as.numeric(IDTOF[x,'TOF']) < 150) {IDTOF[x,'Stage'] <- Stages[2]} 
      else if (as.numeric(IDTOF[x,'TOF']) >= 150 & as.numeric(IDTOF[x,'TOF']) < 225) {IDTOF[x,'Stage'] <- Stages[3]} 
      else if (as.numeric(IDTOF[x,'TOF']) >= 225 & as.numeric(IDTOF[x,'TOF']) < 500) {IDTOF[x,'Stage'] <- Stages[4]} 
      else if (as.numeric(IDTOF[x,'TOF']) >= 500 & as.numeric(IDTOF[x,'TOF']) <= 800) {IDTOF[x,'Stage'] <- Stages[5]} 
      else if (as.numeric(IDTOF[x,'TOF']) > 800) {IDTOF[x,'Stage'] <- 'TooBig'}
    } 
    for (x in 1:dim(IDTOF)[1]) { 
      IDTOF[x,'ID'] <- paste('X',IDTOF[x,'ID'],sep='')
    }
    #Removing worms with amplitude of 35000 or higher 
    #Removing worms that are not in the size we care about 
    IDTOF <- IDTOF[-which(IDTOF[,'Stage'] == 'TooSmall'),]
    IDTOF <- IDTOF[-which(IDTOF[,'Stage'] == 'TooBig'),]
    IDTOF <- IDTOF[-which(IDTOF[,'PH.EXT'] > 35000),]
    DataList[[z]] <- IDTOF
  }
  
  
  for (x in 1:length(DataList)) { 
    if (x == 1) {  maxrow <- dim(DataList[[x]])[1]}
    
    if (maxrow < dim(DataList[[x]])[1]) { 
      maxrow <- dim(DataList[[x]])[1]}
  }
  temp <- matrix(nrow=maxrow, ncol= length(DataList))
  temp <- as.data.frame(temp)
  colnames(temp) <- Names
  for (x in 1:length(DataList)) { 
    temp[1:dim(DataList[[x]])[1],Names[x]] <- DataList[[x]][1:dim(DataList[[x]])[1],FluorescenceChannel]
  }
  output <- ggplot(data = melt(temp), aes(x=variable,y=value)) + 
    geom_boxplot(aes(fill=variable)) + 
    ggtitle('Summary') +
    xlab('Strains') + 
    ylab('Fluorescence (A.U)') +
    theme(plot.title = element_text(hjust = 0.5)) + 
    guides(fill=guide_legend(title='Strains'))
return(output)
}

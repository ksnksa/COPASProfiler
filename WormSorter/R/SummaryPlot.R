#' Plots the provided Worm sorter output file
#'
#' The function takes in the data set directory, the name of the strain and plots the desired fluorescence channel.
#' The IDsFile parameter is optional. If the user provides it, the function will remove the bad worms from the
#' data set before plotting. The function returns five plots: 1- Worms with TOF from 50-75 2- Worms with TOF 75-150
#' 3- Worms with TOF 150-225 4- Worms with TOF 225-500 5- Worms with TOF from 500-800 6- Box plot of the five TOF groups 
#'
#' @param FileDirectory Directory to the worm sorter summary output file. 
#' @param Name Name of the strain that will be used in the plots.
#' @param FluorescenceChannel Which channel to plot: G is green, R is red, and Y is yellow. 
#' @param IDsFile Optional input: Directory to the annotated IDs file to remove the specified bad worms from the data set.
#' 
#' @return Returns 6 plots in  a list. 
#' @export
SummaryPlot <- function(FileDirectory, Name, FluorescenceChannel,IDsFile) { 
  ### If statements to check its the correct file type and is not empty 
  if(missing(FileDirectory)){
    stop('Missing FileDirectory input')
  }  else if(typeof(FileDirectory) != 'character') {
    stop('FileDirectory type is expected to be character, please provide the correct input type')
  } else if(missing(Name)){
    stop('Missing Name input')
  }  else if(typeof(Name) != 'character') {
    stop('Name type is expected to be character, please provide the correct input type')
  } else if(missing(FluorescenceChannel)){
    stop('Missing FluorescenceChannel input')
  }  else if(typeof(FluorescenceChannel) != 'character') {
    stop('FluorescenceChannel type is expected to be character, please provide the correct input type')
  } else if(missing(IDsFile)){
    if(!require("ggplot2")){
      library("ggplot2")
    }
    if(!require("dplyr")){
      library("dplyr")
    }
    if(!require("reshape2")){
      library("reshape2")
    }
    ChannelSummary <- read.delim(FileDirectory, header=TRUE)
    IDTOF <- matrix(nrow=which.max(ChannelSummary[,1]),ncol=8)
    IDTOF <- data.frame(IDTOF)
    colnames(IDTOF) <- c('ID','TOF','EXT','G','Y','R','PH.EXT','Stage')
    IDTOF[,1] <- ChannelSummary[1:dim(IDTOF)[1],1]
    IDTOF[,2] <- ChannelSummary[1:dim(IDTOF)[1],10]
    IDTOF[,3] <- ChannelSummary[1:dim(IDTOF)[1],11]
    IDTOF[,4] <- ChannelSummary[1:dim(IDTOF)[1],12]
    IDTOF[,5] <- ChannelSummary[1:dim(IDTOF)[1],13]
    IDTOF[,6] <- ChannelSummary[1:dim(IDTOF)[1],14]
    IDTOF[,7] <- ChannelSummary[1:dim(IDTOF)[1],15]
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
    IDTOF <- IDTOF %>% mutate(Stage = factor(Stage, levels = Stages))
    if (FluorescenceChannel == 'G') { 
      TOFPlots <- ggplot(IDTOF, aes(Stage,G, label = TOF)) + 
        geom_jitter(width=0.3,alpha=0.2) +
        geom_boxplot(color='green',alpha=1) + 
        ggtitle(Name) + 
        xlab('Stages (by TOF)') + 
        ylab('Fluorescence (A.U)') +
        theme(plot.title = element_text(hjust = 0.5))
    } else if (FluorescenceChannel == 'Y') {TOFPlots <- ggplot(IDTOF, aes(Stage,Y, label = TOF)) + 
      geom_jitter(width=0.3,alpha=1) +
      geom_boxplot(color='yellow',alpha=1) + 
      ggtitle(Name) + 
      xlab('Stages (by TOF)') + 
      ylab('Fluorescence (A.U)') +
      theme(plot.title = element_text(hjust = 0.5))}
    else if (FluorescenceChannel == 'R') {TOFPlots <- ggplot(IDTOF, aes(Stage,R, label = TOF)) + 
      geom_jitter(width=0.3,alpha=1) +
      geom_boxplot(color='red',alpha=1) + 
      ggtitle(Name) + 
      xlab('Stages (by TOF)') + 
      ylab('Fluorescence (A.U)') +
      theme(plot.title = element_text(hjust = 0.5))}
    
    temp <- IDTOF[which(IDTOF[,'Stage'] == Stages[x]),]
    Plots <- list()
    if (FluorescenceChannel == 'G') {
      for (x in 1:length(Stages)) {
        temp <- IDTOF[which(IDTOF[,'Stage'] == Stages[x]),]
        Plots[[x]] <- ggplot(temp, aes(y = G, x = TOF)) + 
          geom_jitter(width=0.3,alpha=1) + 
          ggtitle(paste('Worms with TOF in the range of ',Stages[x], sep = '')) + 
          xlab('TOF') + 
          ylab('Fluorescence (A.U)') +
          theme(plot.title = element_text(hjust = 0.5))
      }
    } else if (FluorescenceChannel == 'Y') {for (x in 1:length(Stages)) {
      temp <- IDTOF[which(IDTOF[,'Stage'] == Stages[x]),]
      Plots[[x]] <- ggplot(temp, aes(y = Y, x = TOF)) + 
        geom_jitter(width=0.3,alpha=1) + 
        ggtitle(paste('Worms with TOF in the range of ',Stages[x], sep = '')) + 
        xlab('TOF') + 
        ylab('Fluorescence (A.U)') +
        theme(plot.title = element_text(hjust = 0.5))
    }}
    else if (FluorescenceChannel == 'R') {for (x in 1:length(Stages)) {
      temp <- IDTOF[which(IDTOF[,'Stage'] == Stages[x]),]
      Plots[[x]] <- ggplot(temp, aes(y = R, x = TOF)) + 
        geom_jitter(width=0.3,alpha=1) + 
        ggtitle(paste('Worms with TOF in the range of ',Stages[x], sep = '')) + 
        xlab('TOF') + 
        ylab('Fluorescence (A.U)') +
        theme(plot.title = element_text(hjust = 0.5))
    }}
    Plots[[6]] <- TOFPlots
    
    return(Plots)
  }  else if(typeof(IDsFile) != 'character') {
    stop('IDsFile type is expected to be character, please provide the correct input type')
  }
  if(!require("ggplot2")){
    library("ggplot2")
  }
  if(!require("dplyr")){
    library("dplyr")
  }
  if(!require("reshape2")){
    library("reshape2")
  }
  ChannelSummary <- read.delim(FileDirectory, header=TRUE)
  IDTOF <- matrix(nrow=which.max(ChannelSummary[,1]),ncol=8)
  IDTOF <- data.frame(IDTOF)
  colnames(IDTOF) <- c('ID','TOF','EXT','G','Y','R','PH.EXT','Stage')
  IDTOF[,1] <- ChannelSummary[1:dim(IDTOF)[1],1]
  IDTOF[,2] <- ChannelSummary[1:dim(IDTOF)[1],10]
  IDTOF[,3] <- ChannelSummary[1:dim(IDTOF)[1],11]
  IDTOF[,4] <- ChannelSummary[1:dim(IDTOF)[1],12]
  IDTOF[,5] <- ChannelSummary[1:dim(IDTOF)[1],13]
  IDTOF[,6] <- ChannelSummary[1:dim(IDTOF)[1],14]
  IDTOF[,7] <- ChannelSummary[1:dim(IDTOF)[1],15]
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
  IDTOF <- IDTOF %>% mutate(Stage = factor(Stage, levels = Stages))
  #removing specific ids 
  GoodIDs <- read.delim(IDsFile, sep=',',header=TRUE)
  BadWormIndex <- which(GoodIDs[2,] == 'Bad')
  BadWormIndex <- unname(GoodIDs[1,BadWormIndex])
  index <- which(IDTOF[,'ID'] %in% BadWormIndex)
  Temp <- IDTOF[-index,]
  IDTOF<- Temp
  if (FluorescenceChannel == 'G') { 
    TOFPlots <- ggplot(IDTOF, aes(Stage,G, label = TOF)) + 
      geom_jitter(width=0.3,alpha=0.2) +
      geom_boxplot(color='green',alpha=1) + 
      ggtitle(Name) + 
      xlab('Stages (by TOF)') + 
      ylab('Fluorescence (A.U)') +
      theme(plot.title = element_text(hjust = 0.5))
  } else if (FluorescenceChannel == 'Y') {TOFPlots <- ggplot(IDTOF, aes(Stage,Y, label = TOF)) + 
    geom_jitter(width=0.3,alpha=1) +
    geom_boxplot(color='yellow',alpha=1) + 
    ggtitle(Name) + 
    xlab('Stages (by TOF)') + 
    ylab('Fluorescence (A.U)') +
    theme(plot.title = element_text(hjust = 0.5))}
  else if (FluorescenceChannel == 'R') {TOFPlots <- ggplot(IDTOF, aes(Stage,R, label = TOF)) + 
    geom_jitter(width=0.3,alpha=1) +
    geom_boxplot(color='red',alpha=1) + 
    ggtitle(Name) + 
    xlab('Stages (by TOF)') + 
    ylab('Fluorescence (A.U)') +
    theme(plot.title = element_text(hjust = 0.5))}
  
    temp <- IDTOF[which(IDTOF[,'Stage'] == Stages[x]),]
    Plots <- list()
    if (FluorescenceChannel == 'G') {
      for (x in 1:length(Stages)) {
      temp <- IDTOF[which(IDTOF[,'Stage'] == Stages[x]),]
      Plots[[x]] <- ggplot(temp, aes(y = G, x = TOF)) + 
        geom_jitter(width=0.3,alpha=1) + 
        ggtitle(paste('Worms with TOF in the range of ',Stages[x], sep = '')) + 
        xlab('TOF') + 
        ylab('Fluorescence (A.U)') +
        theme(plot.title = element_text(hjust = 0.5))
    }
    } else if (FluorescenceChannel == 'Y') {for (x in 1:length(Stages)) {
      temp <- IDTOF[which(IDTOF[,'Stage'] == Stages[x]),]
      Plots[[x]] <- ggplot(temp, aes(y = Y, x = TOF)) + 
        geom_jitter(width=0.3,alpha=1) + 
        ggtitle(paste('Worms with TOF in the range of ',Stages[x], sep = '')) + 
        xlab('TOF') + 
        ylab('Fluorescence (A.U)') +
        theme(plot.title = element_text(hjust = 0.5))
    }}
    else if (FluorescenceChannel == 'R') {for (x in 1:length(Stages)) {
      temp <- IDTOF[which(IDTOF[,'Stage'] == Stages[x]),]
      Plots[[x]] <- ggplot(temp, aes(y = R, x = TOF)) + 
        geom_jitter(width=0.3,alpha=1) + 
        ggtitle(paste('Worms with TOF in the range of ',Stages[x], sep = '')) + 
        xlab('TOF') + 
        ylab('Fluorescence (A.U)') +
        theme(plot.title = element_text(hjust = 0.5))
    }}
    Plots[[6]] <- TOFPlots
  
  return(Plots)
}
